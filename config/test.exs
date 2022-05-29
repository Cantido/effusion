import Config

config :effusion, tracker_worker: Effusion.MockHTTP

config :logger,
  level: :debug

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :effusion_desktop, EffusionDesktopWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "xv1dhOTcLmX32MXAtrrqc7XIG6FkMgLJJ7BmyGrjsp0kUuvgpYQYgmNLagmYDmuV",
  server: false

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
