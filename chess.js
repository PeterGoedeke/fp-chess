const base = require('./base')
const board = require('./board')

const pawn = require('./pieces/pawn')

const initialState = {
    board: [
        ['R', 0, 0, 0, 'P', 0, 'p', 'r'],
        ['N', 'P', 0, 0, 'p', 0, 0, 'n'],
        ['B', 'P', 0, 0, 0, 0, 'p', 'b'],
        ['Q', 'P', 0, 0, 0, 0, 'p', 'q'],
        ['K', 'P', 0, 0, 0, 0, 'p', 'k'],
        ['B', 'P', 0, 0, 0, 0, 'p', 'b'],
        ['N', 'P', 0, 0, 0, 0, 'p', 'n'],
        ['R', 'P', 0, 0, 0, 0, 'p', 'r'],
    ],
    whoseMove: 0,
    enpassant: { i: 1, j: 4 }
}

const nextBoard = state => validMove(state)
    ? applyMove(state)
    : state.board

// const next = spec({
//     whoseMove: !base.prop('whoseMove'),
//     board: nextBoard,
//     move: clear,
//     enpassant: nextEnpassant
// })

const clear = state => null
const nextEnpassant = state => board.pieceToMove(state).toLowerCase() == 'p' // piece to move is a pawn
    && base.pointDistance(state.move[0])(state.move[1]) == 2 // piece is being moved two places
    ? state.move[1]
    : null

    // && state.move[0].j == 1 && state.whoseMove == 1 || state.move[0].j == 6 && state.whoseMove == 0 // 

const validMove = state => state.move
    ? !base.pointEq(state.move[0])(state.move[1])
        ? getCodeAt(state)(...state.move[0])
        : false
    : false

// check if black and if black then flip before pipeing otherwise just pipe

const applyMove = state => base.pipe(
    base.replace2d(...state.move[1])(board.pieceToMove(state)),
    base.replace2d(...state.move[0])(0),
    // check for en passant and apply it here
    // check for castling and apply it here
    // check for queening and apply it here
)(state.board)

function printBoard(board) {
    console.log('----------------------')
    board.forEach(column => console.log(column.join(', ')))
    console.log('----------------------')
}
