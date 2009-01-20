# $Id: REST.pm 30 2008-08-16 00:58:07Z dtikhonov $
# RT::Client::REST
#
# Dmitri Tikhonov <dtikhonov@yahoo.com>
# April 18, 2006
#
# Part of the source is Copyright (c) 2007-2008 Damien Krotkine <dams@cpan.org>
#
# This code is adapted (stolen) from /usr/bin/rt that came with RT.  I just
# wanted to make an actual module out of it.  Therefore, this code is GPLed.
#
# Original notice:
#------------------------
# COPYRIGHT:
# This software is Copyright (c) 1996-2005 Best Practical Solutions, LLC
#                                          <jesse@bestpractical.com>
# Designed and implemented for Best Practical Solutions, LLC by
# Abhijit Menon-Sen <ams@wiw.org>
#------------------------


package RT::Client::REST;

use strict;
use warnings;

use vars qw/$VERSION/;
$VERSION = '0.37';

use Error qw(:try);
use HTTP::Cookies;
use HTTP::Request::Common;
use RT::Client::REST::Exception 0.18;
use RT::Client::REST::Forms;
use RT::Client::REST::HTTPClient;

# Generate accessors/mutators
for my $method (qw(server _cookie timeout)) {
    no strict 'refs';
    *{__PACKAGE__ . '::' . $method} = sub {
        my $self = shift;
        $self->{'_' . $method} = shift if @_;
        return $self->{'_' . $method};
    };
}

sub new {
    my $class = shift;

    $class->_assert_even(@_);

    my $self = bless {}, ref($class) || $class;
    my %opts = @_;

    while (my ($k, $v) = each(%opts)) {
        $self->$k($v);
    }

    return $self;
}

sub login {
    my $self = shift;

    $self->_assert_even(@_);

    my %opts = @_;
    unless (defined($opts{username}) and defined($opts{password})) {
        RT::Client::REST::InvalidParameterValueException->throw(
            "You must provide username and password to log in",
        );
    }

    # OK, here's how login works.  We request to see ticket 1.  We don't
    # even care if it exists.  We watch exceptions: auth. failures and
    # server-side errors we bubble up and ignore all others.
    try {
        $self->_cookie(undef);  # Start a new session.
        $self->_submit("ticket/1", undef, {
            user => $opts{username},
            pass => $opts{password},
        });
    } catch RT::Client::REST::AuthenticationFailureException with {
        shift->rethrow;
    } catch RT::Client::REST::MalformedRTResponseException with {
        shift->rethrow;
    } catch RT::Client::REST::RequestTimedOutException with {
        shift->rethrow;
    } catch RT::Client::REST::HTTPException with {
        shift->rethrow;
    } catch Exception::Class::Base with {
        # ignore others.
    };
}


sub show {
    my $self = shift;

    $self->_assert_even(@_);

    my %opts = @_;

    my $type = $self->_valid_type(delete($opts{type}));
    my $id;

    if (grep { $type eq $_ } (qw(user queue))) {
        # User or queue ID does not have to be numeric
        $id = delete($opts{id});
    } else {
        $id = $self->_valid_numeric_object_id(delete($opts{id}));
    }

    my $form = form_parse($self->_submit("$type/$id")->decoded_content);
    my ($c, $o, $k, $e) = @{$$form[0]};

    if (!@$o && $c) {
        RT::Client::REST::Exception->_rt_content_to_exception($c)->throw;
    }

    return $k;
}

sub get_attachment_ids {
    my $self = shift;

    $self->_assert_even(@_);

    my %opts = @_;

    my $type = $self->_valid_type(delete($opts{type}) || 'ticket');
    my $id = $self->_valid_numeric_object_id(delete($opts{id}));

    my $form = form_parse(
        $self->_submit("$type/$id/attachments/")->decoded_content
    );
    my ($c, $o, $k, $e) = @{$$form[0]};

    if (!@$o && $c) {
        RT::Client::REST::Exception->_rt_content_to_exception($c)->throw;
    }

    return $k->{Attachments} =~ m/(\d+):/mg;
}

sub get_attachment {
    my $self = shift;

    $self->_assert_even(@_);

    my %opts = @_;

    my $type = $self->_valid_type(delete($opts{type}) || 'ticket');
    my $parent_id = $self->_valid_numeric_object_id(delete($opts{parent_id}));
    my $id = $self->_valid_numeric_object_id(delete($opts{id}));

    my $form = form_parse(
        $self->_submit("$type/$parent_id/attachments/$id")->decoded_content
    );
    my ($c, $o, $k, $e) = @{$$form[0]};

    if (!@$o && $c) {
        RT::Client::REST::Exception->_rt_content_to_exception($c)->throw;
    }

    return $k;
}

sub get_transaction_ids {
    my $self = shift;

    $self->_assert_even(@_);

    my %opts = @_;

    my $parent_id = $self->_valid_numeric_object_id(delete($opts{parent_id}));
    my $type = $self->_valid_type(delete($opts{type}) || 'ticket');

    my $path;
    my $tr_type = delete($opts{transaction_type});
    if (!defined($tr_type)) {
        # Gotta catch 'em all!
        $path = "$type/$parent_id/history";
    } elsif ('ARRAY' eq ref($tr_type)) {
        # OK, more than one type.  Call ourselves for each.
        # NOTE: this may be very expensive.
        return sort map {
            $self->get_transaction_ids(
                parent_id => $parent_id,
                transaction_type => $_,
            )
        } map {
            # Check all the types before recursing, cheaper to catch an
            # error this way.
            $self->_valid_transaction_type($_)
        } @$tr_type;
    } else {
        $tr_type = $self->_valid_transaction_type($tr_type);
        $path = "$type/$parent_id/history/type/$tr_type"
    }

    my $form = form_parse( $self->_submit($path)->decoded_content );
    my ($c, $o, $k, $e) = @{$$form[0]};

    if (!length($e)) {
        my $ex = RT::Client::REST::Exception->_rt_content_to_exception($c);
        unless ($ex->message =~ m~^0/~) {
            # We do not throw exception if the error is that no values
            # were found.
            $ex->throw;
        }
    }

    return $e =~ m/^(?:>> )?(\d+):/mg;
}

sub get_transaction {
    my $self = shift;

    $self->_assert_even(@_);

    my %opts = @_;

    my $type = $self->_valid_type(delete($opts{type}) || 'ticket');
    my $parent_id = $self->_valid_numeric_object_id(delete($opts{parent_id}));
    my $id = $self->_valid_numeric_object_id(delete($opts{id}));

    my $form = form_parse(
        $self->_submit("$type/$parent_id/history/id/$id")->decoded_content
    );
    my ($c, $o, $k, $e) = @{$$form[0]};

    if (!@$o && $c) {
        RT::Client::REST::Exception->_rt_content_to_exception($c)->throw;
    }

    return $k;
}

sub search {
    my $self = shift;

    $self->_assert_even(@_);

    my %opts = @_;

    my $type = $self->_valid_type(delete($opts{type}));
    my $query = delete($opts{query});
    my $orderby = delete($opts{orderby});

    my $r = $self->_submit("search/$type", {
        query => $query,
        (defined($orderby) ? (orderby => $orderby) : ()),
    });

    return $r->decoded_content =~ m/^(\d+):/gm;
}

sub edit {
    my $self = shift;
    $self->_assert_even(@_);
    my %opts = @_;

    my $type = $self->_valid_type(delete($opts{type}));

    my $id = delete($opts{id});
    unless ('new' eq $id) {
        $id = $self->_valid_numeric_object_id($id);
    }

    my %set;
    if (defined(my $set = delete($opts{set}))) {
        while (my ($k, $v) = each(%$set)) {
            vpush(\%set, lc($k), $v);
        }
    }
    if (defined(my $text = delete($opts{text}))) {
        $text =~ s/(\n\r?)/$1 /g;
        vpush(\%set, 'text', $text);
    }
    $set{id} = "$type/$id";

    my $r = $self->_submit('edit', {
        content => form_compose([['', [keys %set], \%set]])
    });

    # This seems to be a bug on the server side: returning 200 Ok when
    # ticket creation (for instance) fails.  We check it here:
    if ($r->decoded_content =~ /not/) {
        RT::Client::REST::Exception->_rt_content_to_exception($r->decoded_content)
        ->throw(
            code    => $r->code,
            message => "RT server returned this error: " .  $r->decoded_content,
        );
    }

    if ($r->decoded_content =~ /^#[^\d]+(\d+) (?:created|updated)/) {
        return $1;
    } else {
        RT::Client::REST::MalformedRTResponseException->throw(
            message => "Cound not read ID of the modified object",
        );
    }
}

sub create { shift->edit(@_, id => 'new') }

sub comment {
    my $self = shift;
    $self->_assert_even(@_);
    my %opts = @_;
    my $action = $self->_valid_comment_action(
        delete($opts{comment_action}) || 'comment');
    my $ticket_id = $self->_valid_numeric_object_id(delete($opts{ticket_id}));
    my $msg = $self->_valid_comment_message(delete($opts{message}));

    my @objects = ("Ticket", "Action", "Text");
    my %values  = (
        Ticket      => $ticket_id,
        Action      => $action,
        Text        => $msg,
    );

    if (exists($opts{cc})) {
        push @objects, "Cc";
        $values{Cc} = delete($opts{cc});
    }

    if (exists($opts{bcc})) {
        push @objects, "Bcc";
        $values{Bcc} = delete($opts{bcc});
    }

    my %data;
    if (exists($opts{attachments})) {
        my $files = delete($opts{attachments});
        unless ('ARRAY' eq ref($files)) {
            RT::Client::REST::InvalidParameterValueException->throw(
                "'attachments' must be an array reference",
            );
        }
        push @objects, "Attachment";
        $values{Attachment} = $files;

        for (my $i = 0; $i < @$files; ++$i) {
            unless (-f $files->[$i] && -r _) {
                RT::Client::REST::CannotReadAttachmentException->throw(
                    "File '" . $files->[$i] . "' is not readable",
                );
            }

            my $index = $i + 1;
            $data{"attachment_$index"} = bless([ $files->[$i] ], "Attachment");
        }
    }

    my $text = form_compose([[ '', \@objects, \%values, ]]);
    $data{content} = $text;

    $self->_submit("ticket/$ticket_id/comment", \%data);

    return;
}

sub correspond { shift->comment(@_, comment_action => 'correspond') }

sub merge_tickets {
    my $self = shift;
    $self->_assert_even(@_);
    my %opts = @_;
    my ($src, $dst) = map { $self->_valid_numeric_object_id($_) }
        @opts{qw(src dst)};
    $self->_submit("ticket/merge/$src", { into => $dst});
    return;
}

sub link_tickets {
    my $self = shift;
    $self->_assert_even(@_);
    my %opts = @_;
    my ($src, $dst) = map { $self->_valid_numeric_object_id($_) }
        @opts{qw(src dst)};
    my $ltype = $self->_valid_link_type(delete($opts{link_type}));
    my $del = (exists($opts{'unlink'}) ? 1 : '');

    $self->_submit("ticket/link", {
        id  => $src,
        rel => $ltype,
        to  => $dst,
        del => $del,
    });

    return;
}

sub unlink_tickets { shift->link_tickets(@_, unlink => 1) }

sub _ticket_action {
    my $self = shift;

    $self->_assert_even(@_);

    my %opts = @_;

    my $id = delete $opts{id};
    my $action = delete $opts{action};

    my $text = form_compose([[ '', ['Action'], { Action => $action }, ]]);

    my $form = form_parse(
        $self->_submit("/ticket/$id/take", { content => $text })->decoded_content
    );
    my ($c, $o, $k, $e) = @{$$form[0]};

    if ($e) {
        RT::Client::REST::Exception->_rt_content_to_exception($c)->throw;
    }
}

sub take { shift->_ticket_action(@_, action => 'take') }
sub untake { shift->_ticket_action(@_, action => 'untake') }
sub steal { shift->_ticket_action(@_, action => 'steal') }

sub _submit {
    my ($self, $uri, $content, $auth) = @_;
    my ($req, $data);

    # Did the caller specify any data to send with the request?
    $data = [];
    if (defined $content) {
        unless (ref $content) {
            # If it's just a string, make sure LWP handles it properly.
            # (By pretending that it's a file!)
            $content = [ content => [undef, "", Content => $content] ];
        }
        elsif (ref $content eq 'HASH') {
            my @data;
            foreach my $k (keys %$content) {
                if (ref $content->{$k} eq 'ARRAY') {
                    foreach my $v (@{ $content->{$k} }) {
                        push @data, $k, $v;
                    }
                }
                else { push @data, $k, $content->{$k} }
            }
            $content = \@data;
        }
        $data = $content;
    }

    # Should we send authentication information to start a new session?
    unless ($self->_cookie || $self->basic_auth_cb) {
        unless (defined($auth)) {
            RT::Client::REST::RequiredAttributeUnsetException->throw(
                "You must log in first",
            );
        }
        push @$data, %$auth;
    }

    # Now, we construct the request.
    if (@$data) {
        $req = POST($self->_uri($uri), $data, Content_Type => 'form-data');
    }
    else {
        $req = GET($self->_uri($uri));
    }
    #$session->add_cookie_header($req);
    if ($self->_cookie) {
        $self->_cookie->add_cookie_header($req);
    }

    # Then we send the request and parse the response.
    #DEBUG(3, $req->as_string);
    my $res = $self->_ua->request($req);
    #DEBUG(3, $res->as_string);

    if ($res->is_success) {
        # The content of the response we get from the RT server consists
        # of an HTTP-like status line followed by optional header lines,
        # a blank line, and arbitrary text.

        my ($head, $text) = split /\n\n/, $res->decoded_content(charset => 'none'), 2;
        my ($status, @headers) = split /\n/, $head;
        $text =~ s/\n*$/\n/ if ($text);

        # "RT/3.0.1 401 Credentials required"
	if ($status !~ m#^RT/\d+(?:\S+) (\d+) ([\w\s]+)$#) {
            RT::Client::REST::MalformedRTResponseException->throw(
                "Malformed RT response received from " . $self->server,
            );
        }

        # Our caller can pretend that the server returned a custom HTTP
        # response code and message. (Doing that directly is apparently
        # not sufficiently portable and uncomplicated.)
        $res->code($1);
        $res->message($2);
        $res->content($text);
        #$session->update($res) if ($res->is_success || $res->code != 401);
        if ($res->header('set-cookie')) {
            my $jar = HTTP::Cookies->new;
            $jar->extract_cookies($res);
            $self->_cookie($jar);
        }

        if (!$res->is_success) {
            # We can deal with authentication failures ourselves. Either
            # we sent invalid credentials, or our session has expired.
            if ($res->code == 401) {
                my %d = @$data;
                if (exists $d{user}) {
                    RT::Client::REST::AuthenticationFailureException->throw(
                        code    => $res->code,
                        message => "Incorrect username or password",
                    );
                }
                elsif ($req->header("Cookie")) {
                    # We'll retry the request with credentials, unless
                    # we only wanted to logout in the first place.
                    #$session->delete;
                    #return submit(@_) unless $uri eq "$REST/logout";
                }
            } else {
                RT::Client::REST::Exception->_rt_content_to_exception(
                    $res->decoded_content)
                ->throw(
                    code    => $res->code,
                    message => "RT server returned this error: " .
                               $res->decoded_content,
                );
            }
        }
    } elsif (
        500 == $res->code &&
        # Older versions of HTTP::Response populate 'message', newer
        # versions populate 'content'.  This catches both cases.
        ($res->decoded_content || $res->message) =~ /read timeout/
    ) {
        RT::Client::REST::RequestTimedOutException->throw(
            "Your request to " . $self->server . " timed out",
        );
    } else {
        RT::Client::REST::HTTPException->throw(
            code    => $res->code,
            message => $res->message,
        );
    }

    return $res;
}

sub _ua {
    my $self = shift;

    unless (exists($self->{_ua})) {
        $self->{_ua} = RT::Client::REST::HTTPClient->new(
            agent => $self->_ua_string,
            env_proxy => 1,
        );
        if ($self->timeout) {
            $self->{_ua}->timeout($self->timeout);
        }
        if ($self->basic_auth_cb) {
            $self->{_ua}->basic_auth_cb($self->basic_auth_cb);
        }
    }

    return $self->{_ua};
}

sub basic_auth_cb {
    my $self = shift;

    if (@_) {
        my $sub = shift;
        unless ('CODE' eq ref($sub)) {
            RT::Client::REST::InvalidParameterValueException->throw(
                "'basic_auth_cb' must be a code reference",
            );
        }
        $self->{_basic_auth_cb} = $sub;
    }

    return $self->{_basic_auth_cb};
}

# Not a constant so that it can be overridden.
sub _list_of_valid_transaction_types {
    sort +(qw(
        Create Set Status Correspond Comment Give Steal Take Told
        CustomField AddLink DeleteLink AddWatcher DelWatcher EmailRecord
    ));
}

sub _valid_type {
    my ($self, $type) = @_;

    unless ($type =~ /^[A-Za-z0-9_.-]+$/) {
        RT::Client::REST::InvaildObjectTypeException->throw(
            "'$type' is not a valid object type",
        );
    }

    return $type;
}

sub _valid_objects {
    my ($self, $objects) = @_;

    unless ('ARRAY' eq ref($objects)) {
        RT::Client::REST::InvalidParameterValueException->throw(
            "'objects' must be an array reference",
        );
    }

    return $objects;
}

sub _valid_numeric_object_id {
    my ($self, $id) = @_;

    unless ($id =~ m/^\d+$/) {
        RT::Client::REST::InvalidParameterValueException->throw(
            "'$id' is not a valid numeric object ID",
        );
    }

    return $id;
}

sub _valid_comment_action {
    my ($self, $action) = @_;

    unless (grep { $_ eq lc($action) } (qw(comment correspond))) {
        RT::Client::REST::InvalidParameterValueException->throw(
            "'$action' is not a valid comment action",
        );
    }

    return lc($action);
}

sub _valid_comment_message {
    my ($self, $message) = @_;

    unless (defined($message) and length($message)) {
        RT::Client::REST::InvalidParameterValueException->throw(
            "Comment cannot be empty (specify 'message' parameter)",
        );
    }

    return $message;
}

sub _valid_link_type {
    my ($self, $type) = @_;
    my @types = qw(DependsOn DependedOnBy RefersTo ReferredToBy HasMember
                   MemberOf);

    unless (grep { lc($type) eq lc($_) } @types) {
        RT::Client::REST::InvalidParameterValueException->throw(
            "'$type' is not a valid link type",
        );
    }

    return lc($type);
}

sub _valid_transaction_type {
    my ($self, $type) = @_;

    unless (grep { $type eq $_ } $self->_list_of_valid_transaction_types) {
        RT::Client::REST::InvalidParameterValueException->throw(
            "'$type' is not a valid transaction type.  Allowed types: " .
            join(", ", $self->_list_of_valid_transaction_types)
        );
    }

    return $type;
}

sub _assert_even {
    shift;
    RT::Client::REST::OddNumberOfArgumentsException->throw(
        "odd number of arguments passed") if @_ & 1;
}

sub _rest {
    my $self = shift;
    my $server = $self->server;

    unless (defined($server)) {
        RT::Client::REST::RequiredAttributeUnsetException->throw(
            "'server' attribute is not set",
        );
    }

    return $server . '/REST/1.0';
}

sub _uri { shift->_rest . '/' . shift }

sub _ua_string {
    my $self = shift;
    return ref($self) . '/' . $self->_version;
}

sub _version { $VERSION }

1;

__END__

=pod

=head1 NAME

RT::Client::REST -- talk to RT installation using REST protocol.

=head1 SYNOPSIS

  use Error qw(:try);
  use RT::Client::REST;

  my $rt = RT::Client::REST->new(
    server => 'http://example.com/rt',
    timeout => 30,
  );

  try {
    $rt->login(username => $user, password => $pass);
  } catch Exception::Class::Base with {
    die "problem logging in: ", shift->message;
  };

  try {
    # Get ticket #10
    $ticket = $rt->show(type => 'ticket', id => 10);
  } catch RT::Client::REST::UnauthorizedActionException with {
    print "You are not authorized to view ticket #10\n";
  } catch RT::Client::REST::Exception with {
    # something went wrong.
  };

=head1 DESCRIPTION

B<RT::Client::REST> is B</usr/bin/rt> converted to a Perl module.  I needed
to implement some RT interactions from my application, but did not feel that
invoking a shell command is appropriate.  Thus, I took B<rt> tool, written
by Abhijit Menon-Sen, and converted it to an object-oriented Perl module.

=head1 USAGE NOTES

This API mimics that of 'rt'.  For a more OO-style APIs, please use
L<RT::Client::REST::Object>-derived classes:
L<RT::Client::REST::Ticket> and L<RT::Client::REST::User>.
not implemented yet).

=head1 METHODS

=over

=item new ()

The constructor can take these options (note that these can also
be called as their own methods):

=over 2

=item B<server>

B<server> is a URI pointing to your RT installation.

If you have already authenticated against RT in some other
part of your program, you can use B<_cookie> parameter to supply an object
of type B<HTTP::Cookies> to use for credentials information.

=item B<timeout>

B<timeout> is the number of seconds HTTP client will wait for the
server to respond.  Defaults to LWP::UserAgent's default timeout, which
is 300 seconds.

=item B<basic_auth_cb>

This callback is to provide the HTTP client (based on L<LWP::UserAgent>)
with username and password for basic authentication.  It takes the
same arguments as C<get_basic_credentials()> of LWP::UserAgent and
returns username and password:

  $rt->basic_auth_cb( sub {
    my ($realm, $uri, $proxy) = @_;
    # do some evil things
    return ($username, $password);
  }

=back

=item login (username => 'root', password => 'password')

Log in to RT.  Throws an exception on error.

Usually, if the other side uses basic HTTP authentication, you do not
have to log in, but rather prodive HTTP username and password instead.
See B<basic_auth_cb> above.

=item show (type => $type, id => $id)

Return a reference to a hash with key-value pair specifying object C<$id>
of type C<$type>.

=item edit (type => $type, id => $id, set => { status => 1 })

Set fields specified in parameter B<set> in object C<$id> of type
C<$type>.

=item create (type => $type, set => \%params, text => $text)

Create a new object of type B<$type> and set initial parameters to B<%params>.
For a ticket object, 'text' parameter can be supplied to set the initial
text of the ticket.
Returns numeric ID of the new object.  If numeric ID cannot be parsed from
the response, B<RT::Client::REST::MalformedRTResponseException> is thrown.

=item search (type => $type, query => $query, %opts)

Search for object of type C<$type> by using query C<$query>.  For
example:

  # Find all stalled tickets
  my @ids = $rt->search(
    type => 'ticket',
    query => "Status = 'stalled'",
  );

C<%opts> is a list of key-value pairs:

=over 4

=item B<orderby>

The value is the name of the field you want to sort by.  Plus or minus
sign in front of it signifies ascending order (plus) or descending
order (minus).  For example:

  # Get all stalled tickets in reverse order:
  my @ids = $rt->search(
    type => 'ticket',
    query => "Status = 'stalled'",
    orderby => '-id',
  );

=back

C<search> returns the list of numeric IDs of objects that matched
your query.  You can then use these to retrieve object information
using C<show()> method:

  my @ids = $rt->search(
    type => 'ticket',
    query => "Status = 'stalled'",
  );
  for my $id (@ids) {
    my ($ticket) = $rt->show(type => 'ticket', ids => [$id]);
    print "Subject: ", $t->{Subject}, "\n";
  }

=item comment (ticket_id => $id, message => $message, %opts)

Comment on a ticket with ID B<$id>.
Optionally takes arguments B<cc> and B<bcc> which are references to lists
of e-mail addresses and B<attachments> which is a list of filenames to
be attached to the ticket.

  $rt->comment(
    ticket_id   => 5,
    message     => "Wild thing, you make my heart sing",
    cc          => [qw(dmitri@localhost some@otherdude.com)],
  );

=item correspond (ticket_id => $id, message => $message, %opts)

Add correspondence to ticket ID B<$id>.  Takes optional B<cc>,
B<bcc>, and B<attachments> parameters (see C<comment> above).

=item get_attachment_ids (id => $id)

Get a list of numeric attachment IDs associated with ticket C<$id>.

=item get_attachment (parent_id => $parent_id, id => $id)

Returns reference to a hash with key-value pair describing attachment
C<$id> of ticket C<$parent_id>.  (parent_id because -- who knows? --
maybe attachments won't be just for tickets anymore in the future).

=item get_transaction_ids (parent_id => $id, %opts)

Get a list of numeric IDs associated with parent ID C<$id>.  C<%opts>
have the following options:

=over 2

=item B<type>

Type of the object transactions are associated wtih.  Defaults to "ticket"
(I do not think server-side supports anything else).  This is designed with
the eye on the future, as transactions are not just for tickets, but for
other objects as well.

=item B<transaction_type>

If not specified, IDs of all transactions are returned.  If set to a
scalar, only transactions of that type are returned.  If you want to specify
more than one type, pass an array reference.

Transactions may be of the following types (case-sensitive):

=over 2

=item AddLink

=item AddWatcher

=item Comment

=item Correspond

=item Create

=item CustomField

=item DeleteLink

=item DelWatcher

=item EmailRecord

=item Give

=item Set

=item Status

=item Steal

=item Take

=item Told

=back

=back

=item get_transaction (parent_id => $id, id => $id, %opts)

Get a hashref representation of transaction C<$id> associated with
parent object C<$id>.  You can optionally specify parent object type in
C<%opts> (defaults to 'ticket').

=item merge_tickets (src => $id1, dst => $id2)

Merge ticket B<$id1> into ticket B<$id2>.

=item link_tickets (src => $id1, dst => $id2, link_type => $type)

Create a link between two tickets.  A link type can be one of the following:

=over 2

=item

DependsOn

=item

DependedOnBy

=item

RefersTo

=item

ReferredToBy

=item

HasMember

=item

MemberOf

=back

=item unlink_tickets (src => $id1, dst => $id2, link_type => $type)

Remove a link between two tickets (see B<link_tickets()>)

=item take (id => $id)

Take ticket C<$id>.
This will throw C<RT::Client::REST::AlreadyTicketOwnerException> if you are
already the ticket owner.

=item untake (id => $id)

Untake ticket C<$id>.
This will throw C<RT::Client::REST::AlreadyTicketOwnerException> if Nobody
is already the ticket owner.

=item steal (id => $id)

Steal ticket C<$id>.
This will throw C<RT::Client::REST::AlreadyTicketOwnerException> if you are
already the ticket owner.

=back

=head1 EXCEPTIONS

When an error occurs, this module will throw exceptions.  I recommend
using Error.pm's B<try{}> mechanism to catch them, but you may also use
simple B<eval{}>.  The former will give you flexibility to catch just the
exceptions you want.

Please see L<RT::Client::REST::Exception> for the full listing and
description of all the exceptions.

=head1 LIMITATIONS

Beginning with version 0.14, methods C<edit()> and C<show()> only support
operating on a single object.  This is a conscious departure from semantics
offered by the original tool, as I would like to have a precise behavior
for exceptions.  If you want to operate on a whole bunch of objects, please
use a loop.

=head1 DEPENDENCIES

The following modules are required:

=over 2

=item

Error

=item

Exception::Class

=item

LWP

=item

HTTP::Cookies

=item

HTTP::Request::Common

=back

=head1 SEE ALSO

L<LWP::UserAgent>,
L<RT::Client::REST::Exception>

=head1 BUGS

Most likely.  Please report.

=head1 VARIOUS NOTES

B<RT::Client::REST> does not (at the moment, see TODO file) retrieve forms from
RT server, which is either good or bad, depending how you look at it.

=head1 VERSION

This is version 0.37 of B<RT::Client::REST>.

=head1 AUTHORS

Original /usr/bin/rt was written by Abhijit Menon-Sen <ams@wiw.org>.  rt
was later converted to this module by Dmitri Tikhonov <dtikhonov@yahoo.com>.
In January of 2008, Damien "dams" Krotkine <dams@cpan.org> joined as the
project's co-maintainer.

=head1 LICENSE

Since original rt is licensed under GPL, so is this module.

=cut
