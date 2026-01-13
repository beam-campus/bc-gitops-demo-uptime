defmodule DemoUptime.StatsCollector do
  @moduledoc """
  GenServer that periodically collects BEAM VM statistics and broadcasts
  them via PubSub for real-time updates.

  ## Configuration

  The collector interval can be configured via application env:

      config :demo_uptime, DemoUptime.StatsCollector,
        interval: 1000  # milliseconds (default)

  ## PubSub Integration

  Stats are broadcast to the topic `"stats:updates"` on `DemoUptime.PubSub`.
  The message format is `{:stats_update, stats_map}`.

  Host applications can subscribe:

      Phoenix.PubSub.subscribe(DemoUptime.PubSub, "stats:updates")

  And handle the message:

      def handle_info({:stats_update, stats}, socket) do
        send_update(DemoUptimeWeb.StatsComponent, id: :vm_stats, stats: stats)
        {:noreply, socket}
      end
  """

  use GenServer

  alias DemoUptime.Stats

  @default_interval 1000
  @pubsub DemoUptime.PubSub
  @topic "stats:updates"

  # Public API

  @doc """
  Start the StatsCollector.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Get the current stats immediately.
  """
  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  @doc """
  Get the PubSub server atom.
  """
  def pubsub, do: @pubsub

  @doc """
  Get the topic for stats updates.
  """
  def topic, do: @topic

  # GenServer callbacks

  @impl true
  def init(_opts) do
    interval = Application.get_env(:demo_uptime, __MODULE__, [])
               |> Keyword.get(:interval, @default_interval)

    # Collect initial stats
    stats = Stats.all()

    # Schedule first tick
    schedule_tick(interval)

    {:ok, %{interval: interval, stats: stats}}
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    {:reply, state.stats, state}
  end

  @impl true
  def handle_info(:tick, state) do
    # Collect stats
    stats = Stats.all()

    # Broadcast to PubSub
    Phoenix.PubSub.broadcast(@pubsub, @topic, {:stats_update, stats})

    # Schedule next tick
    schedule_tick(state.interval)

    {:noreply, %{state | stats: stats}}
  end

  # Private

  defp schedule_tick(interval) do
    Process.send_after(self(), :tick, interval)
  end
end
