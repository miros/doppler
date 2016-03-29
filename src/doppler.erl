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

-type state() :: term().
-type methods() :: map().
-type log() :: list().

-record(doppler, {
    state :: state(),
    methods :: methods(),
    log :: log()
}).

-record(doppler_universal, {
    doppler_ref :: pid()
}).

-type init_fun() :: fun(() -> state()).

-type method_return() :: {term(), state()} | {error, term(), state()}.
-type method_fun() :: fun((...) -> method_return()).


%%====================================================================
%% API functions
%%====================================================================


-spec start(init_fun() | state()) -> #doppler_universal{}.
start(InitFun) when is_function(InitFun) ->
    {ok, DopplerRef} = agent:start(fun() -> #doppler{
        state = InitFun(),
        methods = #{},
        log = []
    } end),
    #doppler_universal{
       doppler_ref = DopplerRef
    };

start(InitialState) ->
    start(fun() -> InitialState end).

-spec stop(#doppler_universal{}) -> ok.
stop(#doppler_universal{doppler_ref = DopplerRef}) ->
    ok = agent:stop(DopplerRef).

-spec log(#doppler_universal{}) -> log().
log(#doppler_universal{doppler_ref = DopplerRef}) ->
    agent:get(DopplerRef, fun(#doppler{log = Log}) -> lists:reverse(Log) end).

-spec state(#doppler_universal{}) -> state().
state(#doppler_universal{doppler_ref = DopplerRef}) ->
    agent:get(DopplerRef, fun(#doppler{state = State}) -> State end).

-spec methods(#doppler_universal{}) -> methods().
methods(#doppler_universal{doppler_ref = DopplerRef}) ->
    agent:get(DopplerRef, fun(#doppler{methods = Methods}) -> Methods end).

-spec def(#doppler_universal{}, atom(), method_fun()) -> ok.
def(#doppler_universal{doppler_ref = DopplerRef}, Name, Fun) when is_atom(Name) and is_function(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Key = {Name, Arity},
    ok = agent:update(DopplerRef, fun(#doppler{methods = Methods} = Doppler) ->
        Doppler#doppler{methods = maps:put(Key, Fun, Methods)}
    end).

-spec call(atom(), list()) -> term() | no_return().
call(FunName, Args) ->
    {#doppler_universal{doppler_ref = DopplerRef}, FunArgs} = split_args(Args),
    case agent:get_and_update(DopplerRef, fun(Doppler) -> call(Doppler, FunName, FunArgs) end) of
        {unknown_method, State} -> erlang:error({doppler_undefined_method_called, {doppler_state, State}, {name, FunName}, {args, FunArgs}});
        {bad_return, Return, State} -> erlang:error({doppler_bad_method_return, {doppler_state, State}, {name, FunName}, {args, FunArgs}, {return, Return}});
        {call_error, Error, State} -> erlang:error({doppler_error_in_method, {doppler_state, State}, {name, FunName}, {args, FunArgs}, {error, Error}});
        {custom_call_error, Error} -> erlang:error(Error);
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
                    {error, CustomError, NewState} -> {{custom_call_error, CustomError}, Doppler#doppler{log = [{FunName, FunArgs} | Log], state = NewState}};
                    BadReturn -> {{bad_return, BadReturn, State}, Doppler#doppler{log = [{FunName, FunArgs} | Log]}}
                end
            catch Class:Error ->
                {{call_error, {Class, Error}}, Doppler#doppler{log = [{FunName, FunArgs} | Log]}}
            end
    end.

