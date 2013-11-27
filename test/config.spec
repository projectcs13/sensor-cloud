{include, ["../include/", "../lib/erlastic_search/include/", "../lib/erlson/include/", "../lib/rabbitmq-erlang-client/include/", "../lib/webmachine/include/"]}.
{suites, "../test/", virtualStreamProcess_tests_SUITE}.
{logdir, "../test-results/"}.

