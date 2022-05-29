defmodule EffusionDesktopWeb.TransferLiveTest do
  use EffusionDesktopWeb.ConnCase

  import Phoenix.LiveViewTest
  import EffusionDesktop.TransfersFixtures

  @create_attrs %{info_hash: "some info_hash"}
  @update_attrs %{info_hash: "some updated info_hash"}
  @invalid_attrs %{info_hash: nil}

  defp create_transfer(_) do
    transfer = transfer_fixture()
    %{transfer: transfer}
  end

  describe "Index" do
    setup [:create_transfer]

    test "lists all transfers", %{conn: conn, transfer: transfer} do
      {:ok, _index_live, html} = live(conn, Routes.transfer_index_path(conn, :index))

      assert html =~ "Listing Transfers"
      assert html =~ transfer.info_hash
    end

    test "saves new transfer", %{conn: conn} do
      {:ok, index_live, _html} = live(conn, Routes.transfer_index_path(conn, :index))

      assert index_live |> element("a", "New Transfer") |> render_click() =~
               "New Transfer"

      assert_patch(index_live, Routes.transfer_index_path(conn, :new))

      assert index_live
             |> form("#transfer-form", transfer: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#transfer-form", transfer: @create_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.transfer_index_path(conn, :index))

      assert html =~ "Transfer created successfully"
      assert html =~ "some info_hash"
    end

    test "updates transfer in listing", %{conn: conn, transfer: transfer} do
      {:ok, index_live, _html} = live(conn, Routes.transfer_index_path(conn, :index))

      assert index_live |> element("#transfer-#{transfer.id} a", "Edit") |> render_click() =~
               "Edit Transfer"

      assert_patch(index_live, Routes.transfer_index_path(conn, :edit, transfer))

      assert index_live
             |> form("#transfer-form", transfer: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#transfer-form", transfer: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.transfer_index_path(conn, :index))

      assert html =~ "Transfer updated successfully"
      assert html =~ "some updated info_hash"
    end

    test "deletes transfer in listing", %{conn: conn, transfer: transfer} do
      {:ok, index_live, _html} = live(conn, Routes.transfer_index_path(conn, :index))

      assert index_live |> element("#transfer-#{transfer.id} a", "Delete") |> render_click()
      refute has_element?(index_live, "#transfer-#{transfer.id}")
    end
  end

  describe "Show" do
    setup [:create_transfer]

    test "displays transfer", %{conn: conn, transfer: transfer} do
      {:ok, _show_live, html} = live(conn, Routes.transfer_show_path(conn, :show, transfer))

      assert html =~ "Show Transfer"
      assert html =~ transfer.info_hash
    end

    test "updates transfer within modal", %{conn: conn, transfer: transfer} do
      {:ok, show_live, _html} = live(conn, Routes.transfer_show_path(conn, :show, transfer))

      assert show_live |> element("a", "Edit") |> render_click() =~
               "Edit Transfer"

      assert_patch(show_live, Routes.transfer_show_path(conn, :edit, transfer))

      assert show_live
             |> form("#transfer-form", transfer: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      {:ok, _, html} =
        show_live
        |> form("#transfer-form", transfer: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.transfer_show_path(conn, :show, transfer))

      assert html =~ "Transfer updated successfully"
      assert html =~ "some updated info_hash"
    end
  end
end
