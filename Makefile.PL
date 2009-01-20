use ExtUtils::MakeMaker;

WriteMakefile(
    NAME            => 'RT::Client::REST',
    VERSION_FROM    => 'lib/RT/Client/REST.pm',
    PREREQ_PM       => {
                        'Encode'                => 0,
                        'Error'                 => 0,
                        'Exception::Class'      => 0,
                        'HTTP::Cookies'         => 0,
                        'HTTP::Request::Common' => 0,
                        'LWP'                   => 0,
                        'Params::Validate'      => 0,
                        'Test::Exception'       => 0,
    },
    LICENSE         => 'perl',
);
