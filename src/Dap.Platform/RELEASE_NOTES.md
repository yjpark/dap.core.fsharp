## 0.8.2
* Update with FAKE 1.16.1 and Paket 5.216.0
* Support DotNet Core 3.0

## 0.8.1
* Cleanup logging in bootstrap

## 0.8.0
* log API changes

## 0.7.4
* add M.instant with instant an param

## 0.7.3
* Make App.Start() synchronously waiting start finished

## 0.7.2
* Update TimeSpan Json Encode and Decode

## 0.7.1
* Tweak Clock.ofDateTimeUtc to handle more kinds

## 0.7.0
* Update with FAKE 5.13.3 and Paket 5.203.2

## 0.6.16
* Remove Type Alias to Context

## 0.6.15
* Use ShortGuid for guid

## 0.6.14
* remove IApp<> interface

## 0.6.13
* Update Thoth.Json.Net to 3.0

## 0.6.12
* add StartAsync()

## 0.6.11
* Add M.timeSpan

## 0.6.10
* support bootstrap ICliHook

## 0.6.9
* Logging format tweak
* add support for IHook

## 0.6.8
* log loaded features to logging

## 0.6.7
* add Feature.tryCreate<'feature>

## 0.6.6
* TODO: Release New Version
* Define IBaseApp

## 0.6.5
* Add INeedSetupAsync.OnSetup
* Bugfix with Instant.JsonDecoder

## 0.6.4
* Add Env Req: TryFindService
* Add Registry Service

## 0.6.3
* Include key in service lookup

## 0.6.2
* Cleanup operators: |-|> |=|> |-|-

## 0.6.1
* Change |>> to |-|>

## 0.6.0
* Types definition: Init Update Subscribe Operate
* IActor Interfaces
* IAgent as wrapper of IActor
* IEnv as runtime manager
* Virtual actor support
* Support task based usage for req
* MailboxPlatform to use MailboxProcessor to dispatch IMsg
