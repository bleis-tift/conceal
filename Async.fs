namespace Conceal

type AsyncOperationStatus<'a> =
  | Started
  | Finished of 'a

type Deferred<'a> =
  | HasNotStartedYet
  | InProgress
  | Resolved of 'a

