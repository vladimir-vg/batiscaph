# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :phoenix_app1,
  ecto_repos: [PhoenixApp1.Repo]

# Configures the endpoint
config :phoenix_app1, PhoenixApp1Web.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "dAtIhI/5KMb2PoJsIaxhGFl88gYZd5XRMjZqdxX5etoshRwWHDlhU99hSS2KUCfP",
  render_errors: [view: PhoenixApp1Web.ErrorView, accepts: ~w(html json)],
  pubsub: [name: PhoenixApp1.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
