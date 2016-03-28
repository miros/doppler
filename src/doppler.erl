-module('doppler').

%% API exports
-export([
    start/1,
    stop/1,
    log/1,
    state/1,
    methods/1,
    def/3,
    call/2
]).

-record(doppler, {
    state,
    methods,
    log
}).

-record(doppler_universal, {
    doppler_ref
}).


%%====================================================================
%% API functions
%%====================================================================

start(Fun) when is_function(Fun) ->
    {ok, DopplerRef} = agent:start(fun() -> #doppler{
        state = Fun(),
        methods = #{},
        log = []
    } end),
    #doppler_universal{
       doppler_ref = DopplerRef
    };

start(InitialState) ->
    start(fun() -> InitialState end).

stop(#doppler_universal{doppler_ref = DopplerRef}) ->
    ok = agent:stop(DopplerRef).

log(#doppler_universal{doppler_ref = DopplerRef}) ->
    agent:get(DopplerRef, fun(#doppler{log = Log}) -> lists:reverse(Log) end).

state(#doppler_universal{doppler_ref = DopplerRef}) ->
    agent:get(DopplerRef, fun(#doppler{state = State}) -> State end).

methods(#doppler_universal{doppler_ref = DopplerRef}) ->
    agent:get(DopplerRef, fun(#doppler{methods = Methods}) -> Methods end).

def(#doppler_universal{doppler_ref = DopplerRef}, Name, Fun) when is_atom(Name) and is_function(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Key = {Name, Arity},
    ok = agent:update(DopplerRef, fun(#doppler{methods = Methods} = Doppler) ->
        Doppler#doppler{methods = maps:put(Key, Fun, Methods)}
    end).

call(FunName, Args) ->
    {#doppler_universal{doppler_ref = DopplerRef}, FunArgs} = split_args(Args),
    case agent:get_and_update(DopplerRef, fun(Doppler) -> call(Doppler, FunName, FunArgs) end) of
        {unknown_method, State} -> erlang:error({doppler_undefined_method_called, {doppler_state, State}, {name, FunName}, {args, FunArgs}});
        {bad_return, Return, State} -> erlang:error({doppler_bad_method_return, {doppler_state, State}, {name, FunName}, {args, FunArgs}, {return, Return}});
        {call_error, Error, State} -> erlang:error({doppler_error_in_method, {doppler_state, State}, {name, FunName}, {args, FunArgs}, {error, Error}});
        {result, Result} -> Result
    end.



%%====================================================================
%% Internal functions
%%====================================================================

split_args(Args) ->
    [Doppler | Rest] = lists:reverse(Args),
    {Doppler, lists:reverse(Rest)}.

call(#doppler{state = State, methods = Methods, log = Log} = Doppler, FunName, FunArgs) ->
    Key = {FunName, length(FunArgs) + 1},
    case maps:find(Key, Methods) of
        error ->
            {{unknown_method, State}, Doppler#doppler{log = [{FunName, FunArgs} | Log]}};
        {ok, Method} ->
            try
                case erlang:apply(Method, [State | FunArgs]) of
                    {Result, NewState} -> {{result, Result}, Doppler#doppler{log = [{FunName, FunArgs} | Log], state = NewState}};
                    BadReturn -> {{bad_return, BadReturn, State}, Doppler#doppler{log = [{FunName, FunArgs} | Log]}}
                end
            catch Class:Error ->
                {{call_error, {Class, Error}}, Doppler#doppler{log = [{FunName, FunArgs} | Log]}}
            end
    end.

