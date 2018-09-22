module StateAgent

open Anathema.Core
open System.IO
open Newtonsoft.Json

module private Impl =
    let logPath = 
        [| Directory.GetCurrentDirectory(); "stateLog" |] |> Path.Combine 

    let settings = JsonSerializerSettings()
    settings.Culture <- System.Globalization.CultureInfo.GetCultureInfo("en-GB")
    settings.Formatting <- Formatting.Indented

    let serializer = JsonSerializer.Create(settings)

    let logState (state: WorldState) (path: string) =
        let stream = File.Create (path.ToString())
        let writer = new JsonTextWriter(new StreamWriter(stream))
        writer.CloseOutput <- true

        serializer.Serialize(writer, state)
        writer.Close()
        
    let mailbox filename (inb: MailboxProcessor<WorldState>) =
        let rec loop () = async {
            while inb.CurrentQueueLength > 1 do
                let! _ = inb.Receive()
                ()

            let! mostRecentState = inb.Receive()
            logState mostRecentState (Path.Combine (logPath, filename))

            return! loop ()
        }
        loop ()

let getStateLogger () = 
    Directory.CreateDirectory (Impl.logPath) |> ignore
    let agent = new MailboxProcessor<WorldState> (Impl.mailbox "state.json")
    agent.Start()
    agent