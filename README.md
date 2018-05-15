# Batiscaph [![Join the chat at https://gitter.im/batiscaph/Lobby](https://badges.gitter.im/batiscaph/Lobby.svg)](https://gitter.im/batiscaph/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Become a Patron](https://pp.userapi.com/c846016/v846016003/50faf/0W4mEbYb_5s.jpg)](https://www.patreon.com/bePatron?u=8172578)

Batiscaph is a research device that submerges into ocean and explores unknown space.
That's what this tool does -- submerges into Erlang node and observes how programs work in it.

Batiscaph is a seeing tool. It helps to understand your code.

# Try it out

1. Install [batiscaph probe](https://github.com/vladimir-vg/batiscaph_probe) as a dependency into your application.
   For Elixir: add `{:batiscaph_probe, "~> 0.1.0"}` into deps in `mix.exs` file.
   
   For Erlang: add `{batiscaph_probe, "0.1.0"}` into deps in `rebar.config`.
   Also add `batiscaph_probe` as a dependency into your `*.app.src` file (`applications` list) or start it manually via `application:ensure_all_started(batiscaph_probe).`

2. Start batiscaph server on the same machine.
   Clone repositiory, checkout stable version, run it:
   ```
   git clone https://github.com/vladimir-vg/batiscaph.git
   cd batiscaph
   git checkout v0.1.0
   make run
   ```

3. Start your application.

4. Open batiscaph web UI: http://0.0.0.0:8099/
   
   Choose (connected) instance id, likely it's gonna be your application.

# Storage

By default Batiscaph will store everything in memory.

If you want to store data persistently you should use Clickhouse storage.
In order to use just define two environment variables: 

```
export CLICKHOUSE_DB = batiscaph
export CLICKHOUSE_URL = http://0.0.0.0:8123/
```

Don't forget to create a database with such name in advance.

After defining these variables start batiscaph
and then run following in Erlang shell:

```
batiscaph_cmds:reset_storage().
```

This will erase and create afresh necessary tables in Clickhouse for batiscaph.
From this moment events should be stored persistently.

# Development

In order to work on web UI you need to set up Brunch javascript transpiling.

```
cd frontend
npm install .
make
```

These commands should install all deps, and start a brunch server that will
wait for changes and update js files in `backend/priv/compiled_static`.

# Running tests

Currently tests are broken, working on it.
