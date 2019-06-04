-module(ppi).
-compile(export_all).
-export([v_int/0]).
-inline([v_int/0]).
-import(lists, [map/2, foldl/3]).

% top level comment

-export_type([property/0, proplist/0]). % comment after
% comment below

-type property()  :: atom() | tuple().
-type proplist()  :: [property()].
-type no_colls()    :: [{LogSize :: non_neg_integer(),
                         NoCollections :: non_neg_integer()}].
-type no_slots()    :: 'default' | non_neg_integer().
-type tab_name()    :: term().
-type type()        :: 'bag' | 'duplicate_bag' | 'set'.
-type update_mode() :: 'dirty' | 'new_dirty' | 'saved' | {'error', Reason :: term()}.
-type anno() :: #{atom() := any()}.
-type mapping()     :: {'map', map()} | {'rec', map(), map()}.
-type environment() :: [mapping(),...].
-type list_any() :: list().

-record(ctxt, {single, equal = 0, typed = 0 :: integer()}).

v_int() -> 42.
v_float() -> 42.5.
v_char() -> $A.
v_char_quote() -> $".

v_var(Asd) -> Asd.

v_true() -> true.
v_false() -> false.

v_atom() -> an_atom.
v_atom_space() -> 'an atom'.
v_atom_quote() -> 'an `atom`'.

v_atom_after() -> 'after'.
v_atom_begin() -> 'begin'.
v_atom_case() -> 'case'.
v_atom_catch() -> 'catch'.
v_atom_do() -> 'do'.
v_atom_else() -> 'else'.
v_atom_end() -> 'end'.
v_atom_fn() -> 'fn'.
v_atom_for() -> 'for'.
v_atom_if() -> 'if'.
v_atom_in() -> 'in'.
v_atom_match() -> 'match'.
v_atom_receive() -> 'receive'.
v_atom_try() -> 'try'.
v_atom_when() -> 'when'.
v_atom_def() -> 'def'.
v_atom_cond() -> 'cond'.
v_atom_let() -> 'let'.
v_atom_of() -> 'of'.

v_string_empty() -> "".
v_string_simple() -> "hello world!".
v_string_quote() -> "hello \"world\"!".
v_string_escapes() -> "\thello world!\n".

v_bstring_empty() -> <<"">>.
v_bstring_simple() -> <<"hello world!">>.
v_bstring_quote() -> <<"hello 'world'!">>.
v_bstring_escapes() -> <<"\thello world!\n">>.

l_empty() -> [].
l_one() -> [1].
l_two() -> [1, 2].
l_three() -> [1, 2, 3].
l_improper() -> [1 | 2].
l_improper_two() -> [1, 2 | 3].

l_nested() -> [[]].
l_nested_one() -> [[1]].
l_nested_two() -> [[1], 2].
l_nested_three() -> [1, [2], 3].
l_nested_improper() -> [[1] | 2].

t_empty() -> {}.
t_one() -> {1}.
t_two() -> {1, 2}.
t_three() -> {1, 2, 3}.

m_empty() -> #{}.
m_one() -> #{a => 1}.
m_two() -> #{a => 1, <<"b">> => false}.
m_three() -> #{a => 1, <<"b">> => false, "c" => []}.

m_one_match(A) -> #{a := 1} = A.
m_two_match(A) -> #{a := 1, <<"b">> := false} = A.
m_three_match(A) -> #{a := 1, <<"b">> := false, "c" := []} =A.

m_update(A) -> A#{a => 1}.

o_unary() -> -1 + bnot 1 * not true.

o_add() -> 1 + 2.
o_add_2() -> 1 + 2 + 3.

o_sub() -> 1 - 2.
o_sub_2() -> 1 - 2 - 3.

o_mul() -> 1 * 2.
o_mul_2() -> 1 * 2 * 3.

o_div() -> 1 / 2.
o_div_2() -> 1 / 2 / 3.

o_prec() -> 1 * (2 + 3).

o_bool() -> true andalso false orelse true.
o_bool_prec() -> true andalso (false orelse true).

e_begin() -> begin true end.
e_begin_2() -> begin true, false end.
e_begin_nested() -> begin begin true end end.
e_begin_nested_2() -> begin begin true, false end end.

e_if() -> if true -> ok end.
e_if_elif(A) -> if A -> ok; true -> error end.
e_if_guards() ->
	if
        a, b -> ok;
        a; b -> ok;
        a, b; c -> ok;
        a; b, c -> ok;
        a, b; c, d -> ok
    end.

e_if_guards_else() ->
	if
        a, b -> ok;
        a; b -> ok;
        a, b; c -> ok;
        a; b, c -> ok;
        a, b; c, d -> ok;
        true -> ok
    end.

e_case(A) -> case A of true -> ok; false -> error end.
e_case_guards(A) -> case A of true when A > 5; A < 2 -> ok; false when A > 10 -> error end.

e_fun() ->
    fun(A) -> A + 1 end.

e_fun_clauses() ->
    fun(true) -> ok; (A) -> A + 1 end.

e_fun_named() ->
    fun F(A) -> F(A + 1) end.

e_call_local(Foo) ->
    v_int(),
    v_var(1),
    (v_int())(),
    Foo(1, 2).

e_call_remote(Foo, Bar) ->
    foo:bar(),
    Foo:bar(1),
    foo:Bar(1, 2),
    (v_int()):(v_int())(),
    Foo:Bar(1, 2, 3).

v_rec() -> #ctxt{}.
v_rec_1() -> #ctxt{single=1}.
v_rec_2() -> #ctxt{single=1, equal=2}.
v_rec_3() -> #ctxt{single=1, equal=2, typed=3}.

v_rec_update(Ctxt) -> Ctxt#ctxt{single=1}.
v_rec_index() -> #ctxt.single.

fn_ref(F, A) ->
    fun v_rec/0.

fn_ref_remote(M, F, A) -> 
    fun lists:foldl/3,
    fun M:foldl/3,
    fun lists:F/3,
    fun M:F/3,
    fun M:F/A.

lc(L) -> [I + 1 || I <- L].
lc_filter(L) -> [I + 1 || I <- L, I < 10].
lc_body(L) -> [begin io:format("~p~n", [I]), I + 1 end || I <- L, I < 10].

e_try() -> try 1/0 after ok end.

e_try_case() -> try 1/0 of 1 -> ok; 2 -> error after ok end.

e_try_case_catch() ->
    try 1/0 of
        1 -> ok;
        2 -> error
    catch
        Throw -> Throw;
        throw:Throw -> Throw;
        error:Error -> Error;
        error:Error:Stack -> {Error, Stack};
        exit:Exit -> Exit;
        _:Other -> Other
    end.

e_try_case_catch_after() ->
    try 1/0 of
        1 -> ok;
        2 -> error
    catch
        Throw -> Throw;
        throw:Throw -> Throw;
        error:Error -> Error;
        error:Error:Stack -> {Error, Stack};
        exit:Exit -> Exit;
        _:Other -> Other
    after
        wat
    end.

e_try_catch() ->
    try
        1/0
    catch
        Throw -> Throw;
        throw:Throw -> Throw;
        error:Error -> Error;
        error:Error:Stack -> {Error, Stack};
        exit:Exit -> Exit;
        _:Other -> Other
    end.

e_try_catch_guard() ->
    try
        1/0
    catch
        Throw when Throw == asd -> Throw
    end.

e_try_catch_after() ->
    try
        1/0
    catch
        Throw -> Throw;
        throw:Throw -> Throw;
        error:Error:Stack -> {Error, Stack};
        exit:Exit -> Exit;
        _:Other -> Other
    after
        ok
    end.

e_recv() -> receive a -> ok end.
e_recv_after() -> receive a -> ok after 500 -> error end.
e_recv_only_after() -> receive after 500 -> error end.
e_recv_clauses_after() -> receive a -> ok; b -> error after 500 -> error end.

e_catch() -> catch 1.

v_bin() -> <<>>.

v_bin_binary(F) -> <<F/binary>>.
v_bin_size(F) -> <<F:8>>.
v_bin_bare(F) -> <<F>>.
v_bin_size_type(F) -> <<F:48/integer-signed-big-unit:8, F:48/unit:8-big-signed-integer>>.

bc(List) -> << <<X:1>> || X <- List >>.

