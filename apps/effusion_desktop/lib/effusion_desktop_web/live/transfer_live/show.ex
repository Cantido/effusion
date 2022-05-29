defmodule EffusionDesktopWeb.TransferLive.Show do
  use EffusionDesktopWeb, :live_view

  alias EffusionDesktop.Transfers

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_params(%{"id" => id}, _, socket) do
    {:noreply,
     socket
     |> assign(:page_title, page_title(socket.assigns.live_action))
     |> assign(:transfer, Transfers.get_transfer!(id))}
  end

  defp page_title(:show), do: "Show Transfer"
  defp page_title(:edit), do: "Edit Transfer"
end
