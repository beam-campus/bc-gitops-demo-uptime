defmodule DemoUptime.MixProject do
  use Mix.Project

  @version "0.3.0"

  def project do
    [
      app: :demo_uptime,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "BEAM VM uptime and statistics service with embeddable LiveComponent",
      package: package(),
      docs: docs()
    ]
  end

  def application do
    [
      mod: {DemoUptime.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:phoenix, "~> 1.7.18"},
      {:phoenix_live_view, "~> 1.0"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.7"},
      {:ex_doc, "~> 0.34", only: :dev, runtime: false}
    ]
  end

  defp package do
    [
      name: "demo_uptime",
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/beam-campus/bc-gitops-demo-uptime"
      }
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "CHANGELOG.md"]
    ]
  end
end
