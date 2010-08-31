{application, cushion_tests,
 [{description, "Tests for cushion"},
  {vsn, "devel"},
  {modules, [cushion_couch_api_test, cushion_json_test]},
  {registered, []},
  {applications, [kernel, stdlib, cushion, eunit, eqc]},
  {env, []}]}.
