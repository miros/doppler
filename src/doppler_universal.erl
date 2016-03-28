-module(doppler_universal).

-export(['$handle_undefined_function'/2]).

'$handle_undefined_function'(Func, Args) ->
    doppler:call(Func, Args).

