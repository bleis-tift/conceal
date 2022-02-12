namespace Conceal

open System.IO
open System.Text
open System.Diagnostics

module CodeRunner =
  type RunResult =
    { Output: string
      Error: bool }

  let runAsync (commandPath: string) (code: string) =
    async {
      let fsxPath = Path.GetTempFileName() + ".fsx"
      File.WriteAllText(fsxPath, code, Encoding.UTF8)
      let info = ProcessStartInfo(commandPath, "fsi " + fsxPath)
      info.CreateNoWindow <- true
      info.UseShellExecute <- false
      info.RedirectStandardOutput <- true
      info.RedirectStandardError <- true
      use proc = Process.Start(info)
      do! Async.AwaitTask(proc.WaitForExitAsync())
      return
        match proc.ExitCode with
        | 0 ->
            let result = proc.StandardOutput.ReadToEnd()
            { Output = result; Error = false }
        | _ ->
            let result = proc.StandardError.ReadToEnd()
            { Output = result; Error = true }
    }
