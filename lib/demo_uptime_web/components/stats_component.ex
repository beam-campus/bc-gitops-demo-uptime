defmodule DemoUptimeWeb.StatsComponent do
  @moduledoc """
  A LiveComponent that displays BEAM VM statistics.

  This component can be embedded in host applications via bc_gitops.
  It auto-refreshes every second to show live uptime and stats.

  ## Required assigns from host:
    - `:id` - unique identifier for this component instance

  ## Optional assigns from host:
    - `:host_app` - name of the host application
    - `:theme` - "light" or "dark" (default: "dark")
  """
  use Phoenix.LiveComponent

  alias DemoUptime.Stats

  @refresh_interval 1000

  @impl true
  def mount(socket) do
    socket =
      socket
      |> assign(:stats, Stats.all())
      |> assign_new(:theme, fn -> "dark" end)
      |> assign_new(:host_app, fn -> "unknown" end)

    if connected?(socket) do
      Process.send_after(self(), {:refresh_stats, socket.assigns.id}, @refresh_interval)
    end

    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(assigns)
      |> assign(:stats, Stats.all())

    {:ok, socket}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    {:noreply, assign(socket, :stats, Stats.all())}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class={theme_classes(@theme)} id={"stats-component-#{@id}"} phx-hook="StatsRefresh">
      <div class="p-6">
        <!-- Header -->
        <div class="flex justify-between items-center mb-6">
          <div>
            <h2 class="text-2xl font-bold flex items-center gap-2">
              <svg class="w-6 h-6 text-green-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"/>
              </svg>
              BEAM Stats
            </h2>
            <p class="text-sm opacity-70 mt-1">
              LiveComponent from <span class="font-mono">demo_uptime</span>
            </p>
          </div>
          <div class="flex items-center gap-2">
            <span class="text-xs opacity-50">
              Hosted by: <%= @host_app %>
            </span>
            <button
              phx-click="refresh"
              phx-target={@myself}
              class="p-2 rounded-lg hover:bg-white/10 transition-colors"
              title="Refresh"
            >
              <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"/>
              </svg>
            </button>
          </div>
        </div>

        <!-- Uptime Display -->
        <div class={uptime_card_classes(@theme)}>
          <div class="text-center">
            <p class="text-sm opacity-70 uppercase tracking-wider mb-1">Uptime</p>
            <p class="text-4xl font-mono font-bold text-green-400">
              <%= @stats.uptime.formatted %>
            </p>
            <%= if @stats.uptime.days > 0 do %>
              <p class="text-sm opacity-50 mt-1">
                <%= @stats.uptime.days %> day<%= if @stats.uptime.days != 1, do: "s" %>
              </p>
            <% end %>
          </div>
        </div>

        <!-- Stats Grid -->
        <div class="grid grid-cols-2 gap-4 mt-4">
          <!-- Memory -->
          <div class={stat_card_classes(@theme)}>
            <div class="flex items-center gap-2 mb-3">
              <svg class="w-4 h-4 text-blue-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 3v2m6-2v2M9 19v2m6-2v2M5 9H3m2 6H3m18-6h-2m2 6h-2M7 19h10a2 2 0 002-2V7a2 2 0 00-2-2H7a2 2 0 00-2 2v10a2 2 0 002 2zM9 9h6v6H9V9z"/>
              </svg>
              <span class="text-sm font-medium">Memory</span>
            </div>
            <p class="text-2xl font-bold text-blue-400"><%= @stats.memory.total_mb %> MB</p>
            <div class="mt-2 space-y-1 text-xs opacity-70">
              <div class="flex justify-between">
                <span>Processes:</span>
                <span><%= format_bytes(@stats.memory.processes_bytes) %></span>
              </div>
              <div class="flex justify-between">
                <span>Binary:</span>
                <span><%= format_bytes(@stats.memory.binary_bytes) %></span>
              </div>
              <div class="flex justify-between">
                <span>ETS:</span>
                <span><%= format_bytes(@stats.memory.ets_bytes) %></span>
              </div>
            </div>
          </div>

          <!-- Processes -->
          <div class={stat_card_classes(@theme)}>
            <div class="flex items-center gap-2 mb-3">
              <svg class="w-4 h-4 text-purple-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10"/>
              </svg>
              <span class="text-sm font-medium">Processes</span>
            </div>
            <p class="text-2xl font-bold text-purple-400"><%= @stats.processes.count %></p>
            <div class="mt-2">
              <div class="flex justify-between text-xs opacity-70 mb-1">
                <span>Usage</span>
                <span><%= @stats.processes.usage_percent %>%</span>
              </div>
              <div class={progress_bar_bg_classes(@theme)}>
                <div
                  class="h-full bg-purple-500 rounded-full transition-all duration-300"
                  style={"width: #{min(@stats.processes.usage_percent * 10, 100)}%"}
                />
              </div>
              <p class="text-xs opacity-50 mt-1">Limit: <%= format_number(@stats.processes.limit) %></p>
            </div>
          </div>

          <!-- Schedulers -->
          <div class={stat_card_classes(@theme)}>
            <div class="flex items-center gap-2 mb-3">
              <svg class="w-4 h-4 text-yellow-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"/>
              </svg>
              <span class="text-sm font-medium">Schedulers</span>
            </div>
            <p class="text-2xl font-bold text-yellow-400">
              <%= @stats.system.schedulers_online %>/<%= @stats.system.schedulers %>
            </p>
            <p class="text-xs opacity-50 mt-2">
              <%= @stats.system.logical_processors %> logical CPUs
            </p>
          </div>

          <!-- I/O -->
          <div class={stat_card_classes(@theme)}>
            <div class="flex items-center gap-2 mb-3">
              <svg class="w-4 h-4 text-cyan-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16V4m0 0L3 8m4-4l4 4m6 0v12m0 0l4-4m-4 4l-4-4"/>
              </svg>
              <span class="text-sm font-medium">I/O</span>
            </div>
            <div class="space-y-2">
              <div>
                <p class="text-xs opacity-70">Input</p>
                <p class="text-lg font-bold text-cyan-400"><%= @stats.io.input_mb %> MB</p>
              </div>
              <div>
                <p class="text-xs opacity-70">Output</p>
                <p class="text-lg font-bold text-cyan-300"><%= @stats.io.output_mb %> MB</p>
              </div>
            </div>
          </div>
        </div>

        <!-- System Info Footer -->
        <div class={footer_classes(@theme)}>
          <div class="flex flex-wrap gap-4 text-xs">
            <div>
              <span class="opacity-50">OTP:</span>
              <span class="ml-1 font-mono"><%= @stats.system.otp_release %></span>
            </div>
            <div>
              <span class="opacity-50">ERTS:</span>
              <span class="ml-1 font-mono"><%= @stats.system.erts_version %></span>
            </div>
            <div>
              <span class="opacity-50">GCs:</span>
              <span class="ml-1 font-mono"><%= format_number(@stats.gc.number_of_gcs) %></span>
            </div>
            <div>
              <span class="opacity-50">Node:</span>
              <span class="ml-1 font-mono"><%= @stats.system.node %></span>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  # Theme-aware styling
  defp theme_classes("light"), do: "bg-white text-gray-900 rounded-xl border border-gray-200"
  defp theme_classes(_), do: "bg-gray-800 text-gray-100 rounded-xl border border-gray-700"

  defp uptime_card_classes("light"), do: "bg-gray-50 rounded-lg p-4 border border-gray-200"
  defp uptime_card_classes(_), do: "bg-gray-900/50 rounded-lg p-4 border border-gray-700"

  defp stat_card_classes("light"), do: "bg-gray-50 rounded-lg p-4 border border-gray-200"
  defp stat_card_classes(_), do: "bg-gray-700/50 rounded-lg p-4 border border-gray-600"

  defp progress_bar_bg_classes("light"), do: "h-1.5 bg-gray-200 rounded-full overflow-hidden"
  defp progress_bar_bg_classes(_), do: "h-1.5 bg-gray-600 rounded-full overflow-hidden"

  defp footer_classes("light"), do: "mt-4 pt-4 border-t border-gray-200"
  defp footer_classes(_), do: "mt-4 pt-4 border-t border-gray-700"

  # Formatting helpers
  defp format_bytes(bytes) when bytes >= 1_048_576 do
    "#{Float.round(bytes / 1_048_576, 1)} MB"
  end

  defp format_bytes(bytes) when bytes >= 1024 do
    "#{Float.round(bytes / 1024, 1)} KB"
  end

  defp format_bytes(bytes), do: "#{bytes} B"

  defp format_number(n) when n >= 1_000_000 do
    "#{Float.round(n / 1_000_000, 1)}M"
  end

  defp format_number(n) when n >= 1000 do
    "#{Float.round(n / 1000, 1)}K"
  end

  defp format_number(n), do: "#{n}"
end
