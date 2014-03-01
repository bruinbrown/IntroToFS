


//  ______                _   _                 
// |  ____|              | | (_)                
// | |__ _   _ _ __   ___| |_ _  ___  _ __  ___ 
// |  __| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
// | |  | |_| | | | | (__| |_| | (_) | | | \__ \
// |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|___/

let Add a b =
    a + b


let AddFunc = fun a b -> a + b





let Add5 = Add 5

Add5 7




let (|>) x f = f x


[1..1000]
|> List.filter (fun t -> (t % 2) = 0)
|> List.map (fun t -> t * t)
|> List.filter (fun t -> t < 1000)





let Add5 number = 
    number + 5

let Add7 number =
    number + 7

let Add12 = Add5 >> Add7

let value = Add12 7


// _______                    
// |__   __|                   
//    | |_   _ _ __   ___  ___ 
//    | | | | | '_ \ / _ \/ __|
//    | | |_| | |_) |  __/\__ \
//    |_|\__, | .__/ \___||___/
//        __/ | |              
//       |___/|_|              


type User = { Username : string; Age : int }

let me = { Username = "bruinbrown93"; Age = 21 }

let GetAge user =
    user.Age



type PurchaseDeadline =
    | GivenDate of System.DateTime
    | EndOfDay


let purchase1 = EndOfDay
let purchase = GivenDate(System.DateTime.Now)


let tup = ("Test", 2)


let someNumber = Some 5
let noNumber = None


//  _____      _   _                                    _       _     
// |  __ \    | | | |                                  | |     | |    
// | |__) |_ _| |_| |_ ___ _ __ _ __    _ __ ___   __ _| |_ ___| |__  
// |  ___/ _` | __| __/ _ \ '__| '_ \  | '_ ` _ \ / _` | __/ __| '_ \ 
// | |  | (_| | |_| ||  __/ |  | | | | | | | | | | (_| | || (__| | | |
// |_|   \__,_|\__|\__\___|_|  |_| |_| |_| |_|_|_|\__,_|\__\___|_| |_|


//           _ _   _   _            _   _     _
//     /\   | | | | | | |          | | | |   (_)                      
//    /  \  | | | | |_| |__   ___  | |_| |__  _ _ __   __ _ ___       
//   / /\ \ | | | | __| '_ \ / _ \ | __| '_ \| | '_ \ / _` / __|      
//  / ____ \| | | | |_| | | |  __/ | |_| | | | | | | | (_| \__ \      
// /_/    \_\_|_|  \__|_| |_|\___|  \__|_| |_|_|_| |_|\__, |___/      
//                                                     __/ |          
//                                                    |___/           


match tup with
| ("Test", _) -> printfn "Success"
| (_, 1)
| (_, 2) -> printfn "Small number"
| _ -> printfn "Bad idea"

let parse = false

match tup, parse with
| _, false -> printfn "Not parsing"
| _, true -> printfn "Parsing"



match purchase with
| EndOfDay -> printfn "End of day"
| GivenDate date -> printfn "%A" date



match me with
| { Username = "bruinbrown93"; Age = _ } -> printfn "Cool guy"
| _ -> printfn "Have you met bruinbrown93?"



let numbers = [ 10; 20; 30; 40 ]

match numbers with
| [ w; x; y; z] -> printfn "numbers had 4 elements which sum to %i" (w + x + y + z)
| _ -> printfn "Not 4 numbers"


match numbers with
| x :: xs -> printfn "Head was %i" x
| _ -> printfn "Empty list"



let (|Even|Odd|) a =
    if (a % 2) = 0 then Even else Odd


match 5 with
| Even -> printfn "Even"
//| Odd -> printfn "Odd"

//  _____  _____  _____  
// |  __ \|  __ \|  __ \ 
// | |  | | |  | | |  | |
// | |  | | |  | | |  | |
// | |__| | |__| | |__| |
// |_____/|_____/|_____/ 
//                                        


type CaseRestrictedUsername = CaseRestrictedUsername of string

let CreateUsername username =
    if (username |> String.forall (fun t  -> System.Char.IsLower(t)) ) then 
        Some(CaseRestrictedUsername(username))
    else None

type TestType = { Username : CaseRestrictedUsername; }

let t = { Username = "Test" }

let username = CreateUsername "Anthony Brown"

match username with
| Some x -> { Username = x }
| None -> failwith "Invalid username"




//  _    _       _ _                __      
// | |  | |     (_) |              / _|     
// | |  | |_ __  _| |_ ___    ___ | |_      
// | |  | | '_ \| | __/ __|  / _ \|  _|     
// | |__| | | | | | |_\__ \ | (_) | |       
//  \____/|_| |_|_|\__|___/  \___/|_| 

//  _ __ ___   ___  __   ___ _   _ _ __ ___
// | '_ ` _ \ / _ \/ _` / __| | | | '__/ _ \
// | | | | | |  __/ (_| \__ \ |_| | | |  __/
// |_| |_| |_|\___|\__,_|___/\__,_|_|  \___|


[<Measure>] type Metre

let x = 10<Metre>


let Add5Metres x =
    x + 5<Metre>

Add5Metres x

Add5Metres 5


//
//   ____   ____  _____  
//  / __ \ / __ \|  __ \ 
// | |  | | |  | | |__) |
// | |  | | |  | |  ___/ 
// | |__| | |__| | |     
//  \____/ \____/|_|     
//                       
//                       


type IUser =
    abstract member GetUsername : unit -> string

type BaseUser (username : string) =
    interface IUser with
        member this.GetUsername () =
            username


let u = BaseUser("Anthony") :> IUser

let s = { new IUser with member this.GetUsername () = "Hello" }


u.GetUsername()



//
//   _____                   _               _   _          
//  / ____|                 (_)             | | | |         
// | |     _ __ ___  ___ ___ _ _ __   __ _  | |_| |__   ___ 
// | |    | '__/ _ \/ __/ __| | '_ \ / _` | | __| '_ \ / _ \
// | |____| | | (_) \__ \__ \ | | | | (_| | | |_| | | |  __/
//  \_____|_|  \___/|___/___/_|_| |_|\__, |  \__|_| |_|\___|
//     | |                            __/ |                 
//  ___| |_ _ __ ___  __ _ _ __ ___  |___/                  
// / __| __| '__/ _ \/ _` | '_ ` _ \/ __|                   
// \__ \ |_| | |  __/ (_| | | | | | \__ \                   
// |___/\__|_|  \___|\__,_|_| |_| |_|___/                   
//                                                          
//                                                          



type IMessage =
    interface
    end

type Error = { ErrorMessage:string; ErrorCode:int }
             interface IMessage

type Success = { Status:string }
               interface IMessage

let error = { ErrorMessage = "I'm a teapot"; ErrorCode = 418 } :> IMessage

match error with
| :? Error as e -> printfn "Code: %i\nMessage: %s" e.ErrorCode e.ErrorMessage
| :? Success -> printfn "Success"
| _ -> failwith "Invalid option"




//                                
//     /\                         
//    /  \   ___ _   _ _ __   ___ 
//   / /\ \ / __| | | | '_ \ / __|
//  / ____ \\__ \ |_| | | | | (__ 
// /_/    \_\___/\__, |_| |_|\___|
//                __/ |           
//               |___/            

open System
open System.Net

let asyncResult = 
    async {
        let wc = new WebClient()
        let! content = wc.AsyncDownloadString(Uri("http://www.comp3020.com"))
        return content.Length
    }


let AsyncAdd5 original =
    async {
        let! a = original
        return a + 5
    } |> Async.RunSynchronously


    

let contentLength = AsyncAdd5 asyncResult


//           _ _                            _                              
//     /\   | | |                          | |                             
//    /  \  | | |  _   _  ___  _   _ _ __  | |_ _   _ _ __   ___  ___      
//   / /\ \ | | | | | | |/ _ \| | | | '__| | __| | | | '_ \ / _ \/ __|     
//  / ____ \| | | | |_| | (_) | |_| | |    | |_| |_| | |_) |  __/\__ \     
// /_/    \_\_|_|  \__, |\___/ \__,_|_|     \__|\__, | .__/ \___||___/     
//                  __/ |                        __/ | |                   
//                 |___/        _               |___/|_|                   
//                  | |        | |                   | |                   
//   __ _ _ __ ___  | |__   ___| | ___  _ __   __ _  | |_ ___    _   _ ___ 
//  / _` | '__/ _ \ | '_ \ / _ \ |/ _ \| '_ \ / _` | | __/ _ \  | | | / __|
// | (_| | | |  __/ | |_) |  __/ | (_) | | | | (_| | | || (_) | | |_| \__ \
//  \__,_|_|  \___| |_.__/ \___|_|\___/|_| |_|\__, |  \__\___/   \__,_|___/
//                                             __/ |                       
//                                            |___/                        


#r """..\packages\FSharp.Data.1.1.10\lib\net40\FSharp.Data.dll"""

open FSharp.Data

[<Literal>]
let schema1 = """{ "Name" : "Anthony", "Age" : 21 }"""

[<Literal>]
let schema2 = """{ "Users" : [ { "Username" : "TestUser", "PercentageComplete" : 4.7 }, { "Username" : "TestUser2", "PercentageComplete" : 97.4 } ] }"""

type tschema1 = JsonProvider<schema1>
type tschema2 = JsonProvider<schema2>


let sample1 = tschema1.Parse(schema1)
sample1.Name

let sample2 = tschema2.Parse(schema2)
for user in sample2.Users do
    printfn "%s: %f" user.Username user.PercentageComplete







//   _____                            _        _   _             
//  / ____|                          | |      | | (_)            
// | |     ___  _ __ ___  _ __  _   _| |_ __ _| |_ _  ___  _ __  
// | |    / _ \| '_ ` _ \| '_ \| | | | __/ _` | __| |/ _ \| '_ \ 
// | |___| (_) | | | | | | |_) | |_| | || (_| | |_| | (_) | | | |
//  \_____\___/|_| |_| |_| .__/ \__,_|\__\__,_|\__|_|\___/|_| |_|
// |  ____|              | |           (_)                       
// | |__  __  ___ __  _ _|_|__  ___ ___ _  ___  _ __  ___        
// |  __| \ \/ / '_ \| '__/ _ \/ __/ __| |/ _ \| '_ \/ __|       
// | |____ >  <| |_) | | |  __/\__ \__ \ | (_) | | | \__ \       
// |______/_/\_\ .__/|_|  \___||___/___/_|\___/|_| |_|___/       
//             | |                                               
//             |_|                                               

let a = seq {
    yield "a"
    yield "b"
}



type LoggingBuilder() =
    let log x =
        printfn "%i" x

    member this.Bind(x,f) =
        log x
        f x

    member this.Return(x) =
        x

let log = LoggingBuilder ()





log {
    let! a = 5
    return a
}