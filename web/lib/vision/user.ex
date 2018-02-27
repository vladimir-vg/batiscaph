defmodule Vision.User do
  use Ecto.Schema
  import Ecto.Changeset
  alias Vision.User


  schema "users" do
    field :probe_access_key, :string

    timestamps()
  end

  # returns access key in proper format
  # which would allow versioning of the auth mechanism
  def access_key(user) do
    "0:#{user.id}:#{user.probe_access_key}"
  end

  @doc false
  def changeset(%User{} = user, attrs) do
    user
    |> cast(attrs, [:probe_access_key])
    |> validate_required([:probe_access_key])
  end
end
