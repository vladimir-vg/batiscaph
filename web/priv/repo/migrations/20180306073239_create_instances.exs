defmodule Vision.Repo.Migrations.CreateInstances do
  use Ecto.Migration

  def change do
    create table(:instances, primary_key: false) do
      add :instance_id, :string, primary_key: true
      add :user_id, references(:users)

      timestamps()
    end

  end
end
