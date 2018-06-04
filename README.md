rebar3_gen
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_gen, ".*", {git, "git@host:user/rebar3_gen.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_gen
    ===> Fetching rebar3_gen
    ===> Compiling rebar3_gen
    <Plugin Output>
