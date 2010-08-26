{application, cushion_tests,
 [{description, "Tests for cushion"},
  {vsn, "devel"},
  {modules, [cushion_couch_api_test]},
  {registered, []},
  {applications, [kernel, stdlib, cushion, eunit]},
  {env, []}]}.
