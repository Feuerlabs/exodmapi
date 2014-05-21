%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%     Read a .exodmrc file
%%% @end
%%% Created : 21 May 2014 by Tony Rogvall <tony@rogvall.se>

-module(exodm_rc).
-export([read/0, read/1]).
-export([read_file/1]).

%% read from $HOME/.exodmrc
read() ->
    Dir = case get(rcdir) of
	      undefined -> os:getenv("HOME");
	      D-> D
	  end,
    read(Dir).

%% read from Dir/.exodmrc
read(false) -> read(".");
read("") ->    read(".");
read(Dir) -> 
    read_file(filename:join(Dir, ".exodmrc")).

%% read from FileName
read_file(FileName) ->
    case file:read_file(FileName) of
	{ok,Bin} -> parse(scan(Bin));
	Error -> Error
    end.

parse(Tokens) ->
    parse(Tokens, []).

parse([[<<"URL">>,Arg]|Ts], Env) ->
    Value = text_expand(binary_to_list(Arg), Env),
    parse(Ts,[{url,Value},{"URL",Value}|Env]);
parse([[<<"USER_ACCOUNT">>,Arg]|Ts],Env) ->
    Value = text_expand(binary_to_list(Arg), Env),
    parse(Ts,[{account,Value},{"USER_ACCOUNT",Value}|Env]);
parse([[<<"USER_AUTH">>,Arg]|Ts], Env) ->
    Value = text_expand(binary_to_list(Arg), Env),
    [User,Pass] = text_split(Value, ":"),
    parse(Ts, [{user,User},{password,Pass},{"USER_AUTH",Value} | Env]);
parse([[<<"ADMIN_AUTH">>,Arg]|Ts], Env) ->
    Value = text_expand(binary_to_list(Arg), Env),
    [User,Pass] = text_split(Value, ":"),
    parse(Ts, [{admin,{User,Pass}},{"ADMIN_AUTH",Value} | Env]);
parse([[Var,Arg]|Ts], Env) ->
    Value = text_expand(binary_to_list(Arg), Env),
    parse(Ts, [{Var,Value} | Env]);
parse([],Env) ->
    lists:reverse(Env).

%% scan a .exodmrc file
%% return [ [Var,Val..] ..]
scan(Bin) ->	    
    lists:foldr(
      fun(Line, Acc) ->
	      case binary:split(Line, <<"#">>) of
		  [<<>>|_] -> Acc;
		  [L | _] -> [binary:split(L,<<"=">>) | Acc]
	      end
      end, [],binary:split(Bin, <<"\n">>, [global])).

%% like binary:split but for "strings"
text_split(String, Splitter) ->
    case string:str(String, Splitter) of
	0 -> [String];
	I -> 
	    {A,B0} = lists:split(I-1,String),
	    B = lists:nthtail(length(Splitter), B0),
	    [A,B]
    end.

%%
%% Utility to exand environment "variables" in unicode text
%% variables are written as ${var} wher var is a encoded atom
%%
text_expand(Text, Env) when is_list(Text) ->
    %% assume unicode character list!
    text_expand_(Text, [], Env);
text_expand(Text, Env) when is_binary(Text) ->
    %% assume utf8 encoded data!
    text_expand_(unicode:characters_to_list(Text), [], Env).

text_expand_([$$,${|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [${,$$], Acc, Env);
text_expand_([$\\,C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([], Acc, _Env) ->
    lists:reverse(Acc).

text_expand_collect_([$}|Text], Var, _Pre, Acc, Env) ->
    Key = rev_variable(Var),
    case lists:keyfind(Key, 1, Env) of
	false -> 
	    text_expand_(Text, Acc, Env);
	{_,Value} ->
	    Acc1 = lists:reverse(Value, Acc),
	    text_expand_(Text, Acc1, Env)
    end;
text_expand_collect_([C|Text], Var, Pre, Acc, Env) ->
    if C >= $a, C =< $z;
       C >= $A, C =< $Z;
       C >= $0, C =< $9;
       C == $_; C == $@;
       C == $\s; C == $\t -> %% space and tab allowed in begining and end
	    text_expand_collect_(Text, [C|Var], Pre, Acc, Env);
       true ->
	    %% char not allowed in variable named
	    text_expand_(Text,  [C | Var ++ Pre ++ Acc], Env)
    end;
text_expand_collect_([], Var, Pre, Acc, Env) ->
    text_expand_([],  Var ++ Pre ++ Acc, Env).

rev_variable(Var) ->
    trim_hd(lists:reverse(trim_hd(Var))).
    
trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.
