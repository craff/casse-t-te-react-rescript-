# A small puzzle to test (ReScript + React) + (OCaml + Cohttp-eio + caqti)

## Compilation of the frontend:

installation with `npm`:
```
cd frontend
npm install rescript rescript-request rescript-future @rescript/react react react-dom
npm run build
npm run tests
npm run deploy
```

The static file needed to run the front end are in frontend/dist

## Compilation of the backend:

Install ocaml 4.14.0 via opam and run
```
cd backend
opam install dune http uri eio eio_main cohttp-eio caqti ppx_rapper_lwt ppx_rapper.runtime caqti-driver-postgresql yojson magic-mime pacomb zip
dune build
```

You can start the back-end with
```
dune exec src/backend.exe
```

Then, you can open http://localhost:8080/ on your favorite browser

## Details:

The backend provides the static files and the frontend and backend communicate
via the following API:

- POST `/send_problem` with a JSON body containing
  ```
  { left:  /string representing the right member of the problem/,
    right: /string representing the left member of the problem/,
    domain: /array of integer which are all the walut we must use in the
           solution/}
  ```
  A unique id and creation timestamp is stored in the data base.
  Example:
  ```
  { left: "a+b"
    right: "c-3"
    domain; [1,2,3]}
  ```
  This request returns the id of the generated problem as JSON

- GET `/get_problem?id=N`
  to retrive the Nth problem. It returns the same json as above.

- POST /send_solution with a JSON body containing
  ```
  { problem: /the id of the problem/,
    auto: /boolean telling if the solution was produce automatically/,
	env: /the value of the variables in the problem as a dictionnary/}
  ```
  A unique id and creation timestamp is stored in the data base.
  Example:
  ```
  {"problem":7,
   "auto":false,
   "env":{"A":8,"B":2,"C":5,"D":1,"E":4,"F":9,"G":3,"H":7,"I":6}}
  ```
  This request returns "ok".
