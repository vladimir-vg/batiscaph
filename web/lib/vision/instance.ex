defmodule Vision.Instance do
  use Ecto.Schema
  import Ecto.Changeset
  alias Vision.Instance


  @primary_key {:instance_id, :string, []}
  @derive {Phoenix.Param, key: :instance_id}
  schema "instances" do
    field :user_id, :integer

    timestamps()
  end

  @doc false
  def changeset(%Instance{} = instance, attrs) do
    instance
    |> cast(attrs, [:user_id, :id])
    |> validate_required([:user_id, :id])
  end
end
