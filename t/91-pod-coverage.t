use Test::More;
eval "use Test::Pod::Coverage 1.00";

plan skip_all => "Test::Pod::Coverage 1.00 required for testing POD coverage" if $@;

plan tests => 9; # heh, plan9!

pod_coverage_ok("RT::Client::REST");
pod_coverage_ok("RT::Client::REST::Exception");
pod_coverage_ok("RT::Client::REST::Object", {
    trustme => [qw(autoget autosync autostore)],
});
pod_coverage_ok("RT::Client::REST::Ticket");
pod_coverage_ok("RT::Client::REST::User");
pod_coverage_ok("RT::Client::REST::Queue");
pod_coverage_ok("RT::Client::REST::Attachment", {
    private => [qw(can count store search _attributes)],
});
pod_coverage_ok("RT::Client::REST::Transaction", {
    private => [qw(can count store search _attributes)],
});
pod_coverage_ok("RT::Client::REST::SearchResult");

# vim:ft=perl:
