defmodule Vision.Repo do
  use Ecto.Repo, otp_app: :vision

  @doc """
  Dynamically loads the repository url from the
  DATABASE_URL environment variable.
  """
  def init(_, opts) do
    {:ok, Keyword.put(opts, :url, System.get_env("VISION_WEB_POSTGRES_URL"))}
  end
end