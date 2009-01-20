use strict;
use warnings;

use Test::More tests => 14;
use Test::Exception;

use constant METHODS => (
    'new', 'to_form', 'from_form', 'rt_type',
    
    # attrubutes:
    'id', 'name', 'password', 'real_name', 'email_address', 'gecos',
    'comments',
);

BEGIN {
    use_ok('RT::Client::REST::User');
}

my $user;

lives_ok {
    $user = RT::Client::REST::User->new;
} 'User can get successfully created';

for my $method (METHODS) {
    can_ok($user, $method);
}

ok('user' eq $user->rt_type);

# vim:ft=perl:
