const base = require('./base')
const board = require('./board')

const getPawnMoves = state => (i, j) => mat => base.pipe(
    checkForwardTiles(state)(i, j, board.pawnMoveDirectionForTeam(state)(i, j)),
    checkPawnCapture(state)(i, j, 1),
    checkPawnCapture(state)(i, j, -1),
    checkEnpassant(state)(i, j, 1),
    checkEnpassant(state)(i, j, -1)
)(base.replace2d(i, j)('x')(mat))

const checkForwardTiles = state => (i, j, dir) => mat => board.getCodeAt(state)(i, j + 1 * dir) == 0 // can move one?
    ? j == board.pawnStartingLocationForTeam(state)(i, j) && board.getCodeAt(state)(i, j + 2 * dir) == 0 // can move two?
        ? base.replaceBetween(i, j + 1 * dir)(i, j + 3 * dir)(1)(mat)
        : base.replace2d(i, j + 1 * dir)(1)(mat)
    : mat

const checkPawnCapture = state => (i, j, dir) => mat => board.areDifferentTeam(state)(i, j)(i + dir, j + 1)
    ? base.replace2d(i + dir, j + 1)(1)(mat)
    : mat

const checkEnpassant = state => (i, j, dir) => mat => state.enpassant && base.pointEq(state.enpassant)({i: i + dir, j})
    ? base.replace2d(i + dir, j + 1)(1)(mat)
    : mat
module.exports = {
    pawn: getPawnMoves,
}