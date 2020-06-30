const base = require('./base')
const board = require('./board')


const initialState = {
    board: [
        ['R', 'P', 0, 0, 0, 0, 'p', 'r'],
        ['N', 'P', 'p', 0, 0, 0, 'p', 'n'],
        ['B', 'P', 0, 0, 0, 0, 'p', 'b'],
        ['Q', 'P', 0, 0, 0, 0, 'p', 'q'],
        ['K', 'P', 0, 0, 0, 0, 'p', 'k'],
        ['B', 'P', 0, 0, 0, 0, 'p', 'b'],
        ['N', 'P', 0, 0, 0, 0, 'p', 'n'],
        ['R', 'P', 0, 0, 0, 0, 'p', 'r'],
    ],
    whoseMove: 0,
    enpassant: null
}

const nextBoard = state => validMove(state)
    ? applyMove(state)
    : state.board

// const next = spec({
//     whoseMove: !base.prop('whoseMove'),
//     board: nextBoard,
//     move: clear,
//     enPassantable: clear
// })

const clear = state => null

const validMove = state => state.move
    ? !base.pointEq(state.move[0])(state.move[1])
        ? getCodeAt(state)(...state.move[0])
        : false
    : false

// check if black and if black then flip before pipeing otherwise just pipe

const applyMove = state => base.pipe(
    base.replace2d(...state.move[1])(getCodeAt(state)(...state.move[0])),
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
