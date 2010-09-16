{application, cushion_tests,
 [{description, "Tests for cushion"},
  {vsn, "devel"},
  {modules, [cushion_fsm_test, cushion_couch_api_test, cushion_json_test]},
  {registered, []},
  {applications, [kernel, stdlib, cushion, eunit, eqc, tools]},
  {env, []}]}.
