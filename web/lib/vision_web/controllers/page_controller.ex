defmodule VisionWeb.PageController do
  use VisionWeb, :controller

  def index(conn, _params) do
    api_url = System.get_env("VISION_WEB_API_URL")
    render conn, "index.html", api_url: api_url
  end
end
