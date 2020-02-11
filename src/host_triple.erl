%% @author The RabbitMQ team
%% @copyright 2020 Pivotal Software, Inc.
%%
%% @doc
%% This module provides functions to get and manipulate uniform host
%% triples.
%%
%% The main goal is to be able to be able to locate native libraries
%% under the `priv' directory. Indeed, the return lib directories are
%% not dependent to the host triple which set at Erlang/OTP compile
%% time.

-module(host_triple).

-export([this_host/0,
         parse/1,
         normalize/1,
         normalize/2,
         format/1,
         native_lib_dirs/1,
         native_lib_dirs/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type parsed_triple() :: #{arch   := binary(),
                           vendor := binary(),
                           os     := binary(),
                           env    => binary()}.
%% A map representing a parsed host triple.

-type wordsize() :: 4 | 8.
%% The host word size as returned by {@link erlang:system_info/1}.

-spec this_host() -> string().
%% @doc
%% Returns the normalized host triple for the host running this Erlang
%% VM instance.
%%
%% @returns the normalized host triple (string).

this_host() ->
    SystemArch = erlang:system_info(system_architecture),
    Wordsize = erlang:system_info(wordsize),
    normalize(SystemArch, Wordsize).

-spec parse(Triple) ->
    parsed_triple() |
    {error, {not_a_host_triple, Triple}}
      when Triple :: iolist().
%% @doc
%% Parses a string reprensenting a host triple.
%%
%% @param Triple a host triple (string).
%% @returns the parsed host triple, or an tuple if the given host triple
%% was not recognized.

parse(Triple) when is_list(Triple) orelse is_binary(Triple) ->
    case string:lexemes(Triple, "-") of
        [Arch, Vendor, OS] ->
            #{arch   => iolist_to_binary(Arch),
              vendor => iolist_to_binary(Vendor),
              os     => iolist_to_binary(OS)};
        [Arch, Vendor, OS, Env] ->
            #{arch   => iolist_to_binary(Arch),
              vendor => iolist_to_binary(Vendor),
              os     => iolist_to_binary(OS),
              env    => iolist_to_binary(Env)};
        _ ->
            {error, {not_a_host_triple, Triple}}
    end.

-spec normalize
(Triple) ->
    string() |
    {error, {unrecognized_host_triple, Triple}}
      when Triple :: iolist();
(parsed_triple()) ->
    parsed_triple();
({error, {not_a_host_triple, NotTriple}}) ->
    parsed_triple() |
    {error, {unrecognized_host_triple, NotTriple}}
      when NotTriple :: iolist().
%% @doc
%% Normalizes a host triple.
%%
%% The given host triple can be a string or a parsed host triple.
%%
%% @param Triple a host triple (string or map).
%% @returns the normalized host triple in the same form as the given
%% argument, or an tuple if the given host triple was not recognized.

normalize(Triple) ->
    normalize(Triple, undefined).

-spec normalize
(Triple, wordsize() | undefined) ->
    string() |
    {error, {unrecognized_host_triple, Triple}}
      when Triple :: iolist();
(parsed_triple(), wordsize() | undefined) ->
    parsed_triple();
({error, {not_a_host_triple, NotTriple}}, wordsize() | undefined) ->
    parsed_triple() |
    {error, {unrecognized_host_triple, NotTriple}}
      when NotTriple :: iolist().
%% @doc
%% Normalizes a host triple.
%%
%% The given host triple can be a string or a parsed host triple.
%%
%% `Wordsize' is used when the given host triple does not convey that
%% information. This is the case of the host triple returned by {@link
%% erlang:system_info/1} on Microsoft Windows.
%%
%% @param Triple a host triple (string or map).
%% @returns the normalized host triple in the same form as the given
%% argument, or an tuple if the given host triple was not recognized.

normalize(Triple, Wordsize) when is_list(Triple) orelse is_binary(Triple) ->
    case normalize(parse(Triple), Wordsize) of
        {error, _} = Error -> Error;
        Normalized         -> format(Normalized)
    end;
normalize(Triple, Wordsize) when is_map(Triple) ->
    Triple1 = normalize_arch(Triple, Wordsize),
    Triple2 = normalize_vendor(Triple1, Wordsize),
    Triple2;
normalize({error, {not_a_host_triple, NotTriple}}, Wordsize) ->
    case iolist_to_binary(NotTriple) of
        <<"win32">> ->
            %% We have no clue of the real architecture underneath.
            %% Let's assume it's an x86 CPU and consider the word size
            %% to decide between 32-bit and 64-bit.
            Arch = case Wordsize of
                       4 -> <<"i686">>;
                       8 -> <<"x86_64">>
                   end,
            #{arch   => Arch,
              vendor => <<"pc">>,
              os     => <<"windows">>,
              env    => <<"msvc">>};
        _ ->
            {error, {unrecognized_host_triple, NotTriple}}
    end.

normalize_arch(#{arch := <<"amd64">>} = Triple, _) ->
    Triple#{arch => <<"x86_64">>};
normalize_arch(Triple, _) ->
    Triple.

normalize_vendor(#{vendor := <<"apple">>} = Triple, _) ->
    Triple;
normalize_vendor(#{os := <<"windows">>} = Triple, _) ->
    Triple#{vendor => <<"pc">>};
normalize_vendor(Triple, _) ->
    Triple#{vendor => <<"unknown">>}.

-spec format(parsed_triple()) ->
    string().
%% @doc
%% Formats the given parsed host triple as a string.
%%
%% @param Triple a parsed host triple (map).
%% @returns the host triple formatted as a string.

format(#{arch := Arch, vendor := Vendor, os := OS, env := Env}) ->
    lists:flatten(
      io_lib:format(
        "~s-~s-~s-~s", [Arch, Vendor, OS, Env]));
format(#{arch := Arch, vendor := Vendor, os := OS}) ->
    lists:flatten(
      io_lib:format(
        "~s-~s-~s", [Arch, Vendor, OS])).

-spec native_lib_dirs(atom()) ->
    [file:filename_all()] | {error, bad_name}.
%% @doc
%% Computes a list of native lib directories for a given application,
%% based on the host triple of the running Erlang VM instance.
%%
%% @param Application an Erlang application name.
%% @returns the list of computed native lib directories.

native_lib_dirs(Application) ->
    native_lib_dirs(Application, this_host()).

-spec native_lib_dirs(atom(), iolist() | parsed_triple()) ->
    [file:filename_all()] | {error, bad_name}.
%% @doc
%% Computes a list of native lib directories for a given application,
%% based on the given host triple.
%%
%% @param Application an Erlang application name.
%% @param Triple a host triple.
%% @returns the list of computed native lib directories.

native_lib_dirs(Application, Triple)
  when is_list(Triple) orelse is_binary(Triple) ->
    case code:priv_dir(Application) of
        {error, _} = Error ->
            Error;
        PrivDir ->
            HostSpecificPrivDir = filename:join(PrivDir, normalize(Triple)),
            [HostSpecificPrivDir,
             PrivDir]
    end;
native_lib_dirs(Application, Triple) when is_map(Triple) ->
    native_lib_dirs(Application, format(Triple)).

-ifdef(TEST).
parsing_test() ->
    ?assertEqual(#{arch   => <<"arch">>,
                   vendor => <<"vendor">>,
                   os     => <<"os">>},
                 parse("arch-vendor-os")),
    ?assertEqual(#{arch   => <<"arch">>,
                   vendor => <<"vendor">>,
                   os     => <<"os">>,
                   env    => <<"env">>},
                 parse("arch-vendor-os-env")),
    ?assertEqual({error, {not_a_host_triple, "win32"}},
                 parse("win32")),
    ?assertEqual({error, {not_a_host_triple, "random string"}},
                 parse("random string")).

tested_pairs() ->
    #{
      %% erlang:system_info(system_architecture) with FreeBSD's official
      %% Erlang package.
      "amd64-portbld-freebsd13.0" => "x86_64-unknown-freebsd13.0",

      %% erlang:system_info(system_architecture) with Erlang built from
      %% source on FreeBSD.
      "x86_64-unknown-freebsd13.0" => "x86_64-unknown-freebsd13.0",

      %% erlang:system_info(system_architecture) with Homebrew's
      %% official Erlang package on Mac OSX.
      "x86_64-apple-darwin19.0.0" => "x86_64-apple-darwin19.0.0",

      %% erlang:system_info(system_architecture) with Debian's official
      %% Erlang package.
      "x86_64-pc-linux-gnu" => "x86_64-unknown-linux-gnu",

      %% erlang:system_info(system_architecture) with RabbitMQ org.'s
      %% Erlang RPM package.
      "x86_64-redhat-linux-gnu" => "x86_64-unknown-linux-gnu",

      %% erlang:system_info(system_architecture) with Erlang.org's
      %% official Windows installers (32-bit and 64-bit).
      {"win32", 4} => "i686-pc-windows-msvc",
      {"win32", 8} => "x86_64-pc-windows-msvc",

      "i686-unknown-windows" => "i686-pc-windows",

      "random string" => {error, {unrecognized_host_triple, "random string"}}
     }.

normalizing_raw_triple_test() ->
    [case Triple of
         {NotTriple, Wordsize} ->
             ?assertEqual(Expected,
                          normalize(NotTriple, Wordsize));
         _ ->
             ?assertEqual(Expected,
                          normalize(Triple))
     end
     || {Triple, Expected} <- maps:to_list(tested_pairs())].

normalizing_parsed_triple_test() ->
    [case Triple of
         {NotTriple, Wordsize} ->
             case Expected of
                 {error, _} ->
                     ?assertEqual(Expected,
                                  normalize(parse(NotTriple), Wordsize));
                 _ ->
                     ?assertEqual(parse(Expected),
                                  normalize(parse(NotTriple), Wordsize))
             end;
         _ ->
             case Expected of
                 {error, _} ->
                     ?assertEqual(Expected,
                                  normalize(parse(Triple)));
                 _ ->
                     ?assertEqual(parse(Expected),
                                  normalize(parse(Triple)))
             end
     end
     || {Triple, Expected} <- maps:to_list(tested_pairs())].

this_host_test() ->
    ?assertMatch([_ | _], this_host()).

native_lib_dirs_test() ->
    Application = host_triple,
    Triple = "amd64-portbld-freebsd13.0",
    PrivDir = code:priv_dir(Application),
    Expected = [filename:join(PrivDir, normalize(Triple)),
                PrivDir],
    ?assertEqual(Expected, native_lib_dirs(Application, Triple)),
    ?assertEqual(Expected, native_lib_dirs(Application, parse(Triple))).
-endif.
