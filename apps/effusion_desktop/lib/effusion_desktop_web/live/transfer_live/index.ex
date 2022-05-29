defmodule EffusionDesktopWeb.TransferLive.Index do
  use EffusionDesktopWeb, :live_view

  alias EffusionDesktop.Transfers

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :transfers, list_transfers())}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    socket
    |> assign(:page_title, "Edit Transfer")
    |> assign(:transfer, Transfers.get_transfer!(id))
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "New Transfer")
    |> assign(:transfer, %{})
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Listing Transfers")
    |> assign(:transfer, nil)
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    transfer = Transfers.get_transfer!(id)
    {:ok, _} = Transfers.delete_transfer(transfer)

    {:noreply, assign(socket, :transfers, list_transfers())}
  end

  defp list_transfers do
    Transfers.list_transfers()
  end
end
