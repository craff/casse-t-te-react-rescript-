
all:
	cd frontend && npm run build np && npm run deploy
	cd backend && dune build

clean:
	cd frontend && npm run clean
	cd backend && dune clean
