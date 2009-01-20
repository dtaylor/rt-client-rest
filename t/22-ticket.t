use strict;
use warnings;

use Test::More tests => 97;
use Test::Exception;

use constant METHODS => (
    'new', 'to_form', 'from_form', 'rt_type', 'comment', 'correspond',
    'attachments', 'transactions', 'take', 'untake', 'steal',
    
    # attrubutes:
    'id', 'queue', 'owner', 'creator', 'subject', 'status', 'priority',
    'initial_priority', 'final_priority', 'requestors', 'cc', 'admin_cc',
    'created', 'starts', 'started', 'due', 'resolved', 'told',
    'time_estimated', 'time_worked', 'time_left', 'last_updated',
);

BEGIN {
    use_ok('RT::Client::REST::Ticket');
}

my $ticket;

lives_ok {
    $ticket = RT::Client::REST::Ticket->new;
} 'Ticket can get successfully created';

for my $method (METHODS) {
    can_ok($ticket, $method);
}

for my $method (qw(comment correspond)) {
    # Need local copy.
    my $ticket = RT::Client::REST::Ticket->new;

    throws_ok {
        $ticket->$method(1);
    } 'RT::Client::REST::Exception'; # Make sure exception inheritance works

    throws_ok {
        $ticket->$method(1);
    } 'RT::Client::REST::Object::OddNumberOfArgumentsException';

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::Object::RequiredAttributeUnsetException',
        "won't go on without RT object";

    throws_ok {
        $ticket->rt('anc');
    } 'RT::Client::REST::Object::InvalidValueException',
        "'rt' expects an actual RT object";

    lives_ok {
        $ticket->rt(RT::Client::REST->new);
    } "RT object successfully set";

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::Object::RequiredAttributeUnsetException',
        "won't go on without 'id' attribute";

    lives_ok {
        $ticket->id(1);
    } "'id' successfully set to a numeric value";

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::Object::InvalidValueException';

    lives_ok {
        $ticket->id(1);
    } "'id' successfully set to a numeric value";

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::Object::InvalidValueException',
        "Need 'message' to $method";

    throws_ok {
        $ticket->$method(message => 'abc');
    } 'RT::Client::REST::RequiredAttributeUnsetException';

    throws_ok {
        $ticket->$method(
            message => 'abc',
            attachments => ['- this file does not exist -'],
        );
    } 'RT::Client::REST::CannotReadAttachmentException';
}

for my $method (qw(attachments transactions)) {
    # Need local copy.
    my $ticket = RT::Client::REST::Ticket->new;

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::Object::RequiredAttributeUnsetException',
        "won't go on without RT object";

    throws_ok {
        $ticket->rt('anc');
    } 'RT::Client::REST::Object::InvalidValueException',
        "'rt' expects an actual RT object";

    lives_ok {
        $ticket->rt(RT::Client::REST->new);
    } "RT object successfully set";

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::Object::RequiredAttributeUnsetException',
        "won't go on without 'id' attribute";

    lives_ok {
        $ticket->id(1);
    } "'id' successfully set to a numeric value";

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::RequiredAttributeUnsetException';
}

for my $method (qw(take untake steal)) {
    # Need local copy.
    my $ticket = RT::Client::REST::Ticket->new;

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::Object::RequiredAttributeUnsetException',
        "won't go on without RT object";

    throws_ok {
        $ticket->rt('anc');
    } 'RT::Client::REST::Object::InvalidValueException',
        "'rt' expects an actual RT object";

    lives_ok {
        $ticket->rt(RT::Client::REST->new);
    } "RT object successfully set";

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::Object::RequiredAttributeUnsetException',
        "won't go on without 'id' attribute";

    lives_ok {
        $ticket->id(1);
    } "'id' successfully set to a numeric value";

    throws_ok {
        $ticket->$method;
    } 'RT::Client::REST::RequiredAttributeUnsetException';
}

# Test list attributes:
my @emails = qw(dmitri@localhost dude@localhost);
throws_ok {
    $ticket->requestors(@emails);
} 'RT::Client::REST::Object::InvalidValueException',
    'List attributes (requestors) only accept array reference';

lives_ok {
    $ticket->requestors(\@emails);
} 'Set requestors to list of two values';

ok(2 == $ticket->requestors, 'There are 2 requestors');

lives_ok {
    $ticket->add_requestors(qw(xyz@localhost root pgsql));
} 'Added three more requestors';

ok(5 == $ticket->requestors, 'There are now 5 requestors');

lives_ok {
    $ticket->delete_requestors('root');
} 'Deleted a requestor (root)';

ok(4 == $ticket->requestors, 'There are now 4 requestors');

ok('ticket' eq $ticket->rt_type);

# vim:ft=perl:
