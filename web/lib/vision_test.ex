defmodule Vision.Test do
  @moduledoc """
  This modules provides helpers that should be called by test runner.
  """

  def create_user_and_return_access_key() do
    {:ok, user} = Vision.Repo.insert(%Vision.User{probe_access_key: Ecto.UUID.generate()})
    {:ok, user.id, Vision.User.access_key(user)}
  end
end
