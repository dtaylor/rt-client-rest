use strict;
use warnings;

use Test::More tests => 26;
use Test::Exception;

use constant METHODS => (
    'new', 'server', 'show', 'edit', 'login',
    'create', 'comment', 'correspond', 'merge_tickets', 'link_tickets',
    'unlink_tickets', 'search', 'get_attachment_ids', 'get_attachment',
    'get_transaction_ids', 'get_transaction', 'take', 'untake', 'steal',
    'timeout', 'basic_auth_cb',
);

use RT::Client::REST;

my $rt;

lives_ok {
    $rt = RT::Client::REST->new;
} 'RT::Client::REST instance created';

for my $method (METHODS) {
    can_ok($rt, $method);
}

throws_ok {
    $rt->login;
} 'RT::Client::REST::InvalidParameterValueException',
    "requires 'username' and 'password' parameters";

throws_ok {
    $rt->basic_auth_cb(1);
} 'RT::Client::REST::InvalidParameterValueException';

throws_ok {
    $rt->basic_auth_cb({});
} 'RT::Client::REST::InvalidParameterValueException';

lives_ok {
    $rt->basic_auth_cb(sub {});
};

# vim:ft=perl:
