%%
%% clexical_rules.hrl — Records for the automation rule engine.
%%
%% A RULE is a #letter{} — the complete script for an automation:
%%
%%   subject    = rule ID (e.g. <<"auto-escalate-001">>)
%%   author     = system or creator identity
%%   predicates = ordered list of trigger + condition + action predicates
%%   type       = decree
%%
%% Predicate naming convention (action field, binary part):
%%
%%   Trigger     on:fieldUpdate | on:statusChange | on:create | on:schedule
%%   Condition   if:fieldValue  | if:status | if:actor | if:relatedItem | if:script
%%   Action      do:fieldUpdate | do:transitionStatus | do:notify |
%%               do:webhook     | do:linkAction        | do:script
%%
%% The letter's predicate list is the "script" that runs in order:
%%   triggers  → conditions (prepositions) → actions (verbs)
%%

%% Persisted automation rule.
-record(rule, {
    id          :: binary(),
    name        :: binary(),
    description = <<>> :: binary(),
    enabled     = true :: boolean(),
    letter      :: any(),           %% #letter{} — the full rule script
    created_at  :: pos_integer(),
    created_by  :: binary()
}).

%% Execution context passed through condition evaluation and action dispatch.
%% Populated from the incoming trigger event letter + adjectives.
-record(ctx, {
    item_id     :: binary(),        %% subject of the trigger event
    actor       :: binary(),        %% author of the trigger event (initiating user)
    fields      = #{} :: map(),     %% current field values  {field => value}
    status      = <<>> :: binary(), %% current status
    trigger     :: binary(),        %% trigger action name  (e.g. <<"on:fieldUpdate">>)
    event_adjs  = #{} :: map(),     %% raw adjectives from the trigger predicate
    rule_id     :: binary(),        %% rule being evaluated
    extra       = #{} :: map()      %% caller-supplied extras (relation maps, etc.)
}).

%% Audit trail entry — one row per rule execution attempt.
-record(audit_entry, {
    id           :: binary(),
    rule_id      :: binary(),
    rule_name    :: binary(),
    triggered_at :: pos_integer(),  %% epoch ms
    actor        :: binary(),
    item_id      :: binary(),
    trigger      :: binary(),
    result       :: success | {error, term()},
    steps        :: [map()]         %% per-predicate execution log
}).
