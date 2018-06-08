# rebar3_gen
A rebar plugin that generates .erl/.hrl (or any other source files) via templates, fetching data from specified URLs.

## Templates
This plugin uses [{{mustache}}](https://github.com/soranoba/bbmustache) template engine to generate files.
Each .gen source file must contain data URL at first line, in form of `{{!https://PATH/TO/JSON}}` comment line.
After data is fetched, it is converted according to it's Content-Type headers (currenctly JSON is supported) and passed to template.
There is a way of parsing and applying custom modifications to fetched data via .script file of same name.
It should contain valid Erlang code and will be called with `Data` binding of fetched data.
_If no URL is provided, template will be called with empty list `[]` as input_

## Build
    $ rebar3 compile

## Use
Add the plugin to your rebar config:

    {plugins, [
        {rebar3_gen, {git, "https://github.com/furmanOFF/rebar3_gen.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 gen
    ===> Fetching rebar3_gen
    ===> Compiling rebar3_gen
    ===> Downloading <URI1>
    ===> Evaluating <FILE1>.script
    ===> Writing out <FILE1>

To have it invoked automatically when running `rebar3 compile` add it as a `provider_hooks`:

    {provider_hooks, [
        {pre, [{compile, gen}]}
    ]}.

## TODO
* Examples
* Additional URIs (e.g. file://)
* Additional Content-Types (e.g. XML, YAML)
* Customize project search path
* Customize .gen file extension
* Multiple data inputs in .gen file