defmodule VisionWeb.PageController do
  use VisionWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
