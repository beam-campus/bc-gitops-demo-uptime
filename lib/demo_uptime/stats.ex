defmodule DemoUptime.Stats do
  @moduledoc """
  Collects BEAM VM statistics.
  """

  @doc """
  Get all VM statistics.
  """
  def all do
    %{
      uptime: uptime(),
      memory: memory(),
      processes: processes(),
      system: system(),
      io: io_stats(),
      gc: gc_stats()
    }
  end

  @doc """
  Get uptime information.
  """
  def uptime do
    {wall_clock, _} = :erlang.statistics(:wall_clock)
    {runtime, _} = :erlang.statistics(:runtime)

    seconds = div(wall_clock, 1000)
    days = div(seconds, 86400)
    hours = div(rem(seconds, 86400), 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    %{
      wall_clock_ms: wall_clock,
      runtime_ms: runtime,
      formatted: format_uptime(days, hours, minutes, secs),
      days: days,
      hours: hours,
      minutes: minutes,
      seconds: secs
    }
  end

  defp format_uptime(0, h, m, s) do
    :io_lib.format("~2..0B:~2..0B:~2..0B", [h, m, s]) |> IO.iodata_to_binary()
  end

  defp format_uptime(d, h, m, s) do
    :io_lib.format("~Bd ~2..0B:~2..0B:~2..0B", [d, h, m, s]) |> IO.iodata_to_binary()
  end

  @doc """
  Get memory statistics.
  """
  def memory do
    mem = :erlang.memory()

    %{
      total_bytes: mem[:total],
      total_mb: round_to(mem[:total] / 1_048_576, 2),
      processes_bytes: mem[:processes],
      processes_used_bytes: mem[:processes_used],
      system_bytes: mem[:system],
      atom_bytes: mem[:atom],
      atom_used_bytes: mem[:atom_used],
      binary_bytes: mem[:binary],
      code_bytes: mem[:code],
      ets_bytes: mem[:ets]
    }
  end

  @doc """
  Get process statistics.
  """
  def processes do
    count = :erlang.system_info(:process_count)
    limit = :erlang.system_info(:process_limit)

    %{
      count: count,
      limit: limit,
      usage_percent: round_to(count / limit * 100, 2)
    }
  end

  @doc """
  Get system information.
  """
  def system do
    %{
      otp_release: :erlang.system_info(:otp_release) |> List.to_string(),
      erts_version: :erlang.system_info(:version) |> List.to_string(),
      schedulers: :erlang.system_info(:schedulers),
      schedulers_online: :erlang.system_info(:schedulers_online),
      logical_processors: :erlang.system_info(:logical_processors),
      atom_count: :erlang.system_info(:atom_count),
      atom_limit: :erlang.system_info(:atom_limit),
      port_count: :erlang.system_info(:port_count),
      port_limit: :erlang.system_info(:port_limit),
      ets_count: length(:ets.all()),
      node: Node.self() |> Atom.to_string()
    }
  end

  @doc """
  Get I/O statistics.
  """
  def io_stats do
    {{:input, input}, {:output, output}} = :erlang.statistics(:io)

    %{
      input_bytes: input,
      output_bytes: output,
      input_mb: round_to(input / 1_048_576, 2),
      output_mb: round_to(output / 1_048_576, 2)
    }
  end

  @doc """
  Get garbage collection statistics.
  """
  def gc_stats do
    {number_of_gcs, words_reclaimed, _} = :erlang.statistics(:garbage_collection)

    %{
      number_of_gcs: number_of_gcs,
      words_reclaimed: words_reclaimed
    }
  end

  defp round_to(number, decimals) do
    p = :math.pow(10, decimals)
    round(number * p) / p
  end
end
