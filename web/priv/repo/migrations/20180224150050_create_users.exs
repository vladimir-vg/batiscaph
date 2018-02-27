defmodule Vision.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :probe_access_key, :string

      timestamps()
    end

  end
end
