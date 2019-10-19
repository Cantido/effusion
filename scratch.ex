{:badarg,
  [
    {
      :erlang,
      :apply,
      [
        {
          {{99, 203, 27, 237}, 51413},
          %{address: {{99, 203, 27, 237}, 51413}, am_choking: true, am_interested: false, failcount: -1, handshaken: false, has: IntSet<[]>, info_hash: <<210, 229, 63, 182, 3, 101, 45, 153, 25, 145, 182, 173, 35, 87, 167, 162, 132, 90, 83, 25>>, peer_choking: true, peer_id: "Effusion Experiment!", peer_interested: false, remote_peer_id: "-TR2920-lgdtn2sb46r0", session: #PID<0.215.0>}
        },
        :peer_id, []], []}, {Effusion.Application.ConnectionSupervisor, :start_child, 1,

        [file: 'lib/effusion/application/connection_supervisor.ex', line: 14]}, {Effusion.BTP.DownloadServer, :handle_internal_message, 2, [file: 'lib/effusion/btp/download_server.ex', line: 70]}, {Enum, :"-each/2-lists^foreach/1-0-", 2, [file: 'lib/enum.ex', line: 783]}, {Enum, :each, 2, [file: 'lib/enum.ex', line: 783]}, {Effusion.BTP.DownloadServer, :handle_cast, 2, [file: 'lib/effusion/btp/download_server.ex', line: 125]}, {:gen_server, :try_dispatch, 4, [file: 'gen_server.erl', line: 637]}, {:gen_server, :handle_msg, 6, [file: 'gen_server.erl', line: 711]}]}
