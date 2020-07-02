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

const getRookMoves = state => (i, j) => mat => checkOrthogonalDirections(state)(i, j)(base.replace2d(i, j)('x')(mat))
const getBishopMoves = state => (i, j) => mat => checkDiagonalDirections(state)(i, j)(base.replace2d(i, j)('x')(mat))

const getQueenMoves = state => (i, j) => mat => base.pipe(
    checkOrthogonalDirections(state)(i, j),
    checkDiagonalDirections(state)(i, j)
)(base.replace2d(i, j)('x')(mat))


const checkOrthogonalDirections = state => (i, j) => mat => base.pipe(
    checkDirection(state)(i, j, -1, 0),
    checkDirection(state)(i, j, 1, 0),
    checkDirection(state)(i, j, 0, -1),
    checkDirection(state)(i, j, 0, 1)
)(mat)

const checkDiagonalDirections = state => (i, j) => mat => base.pipe(
    checkDirection(state)(i, j, 1, 1),
    checkDirection(state)(i, j, -1, -1),
    checkDirection(state)(i, j, 1, -1),
    checkDirection(state)(i, j, -1, 1)
)(mat)
    
const checkDirection = state => (i, j, dI, dJ) => mat => board.areDifferentTeam(state)(i, j)(i + dI, j + dJ)
    ? base.replace2d(i + dI, j + dJ)(1)(mat)
    : board.squareIsEmpty(state)(i + dI, j + dJ)
        ? checkDirection(state)(i, j, dI + base.scalarDirection(dI), dJ + base.scalarDirection(dJ))
            (base.replace2d(i + dI, j + dJ)(1)(mat))
        : mat

const checkSquare = state => (i, j, dI, dJ) => mat => board.areDifferentTeam(state)(i, j)(i + dI, j + dJ) || board.getCodeAt(state)(i + dI, j + dJ) == 0
    ? base.replace2d(i + dI, j + dJ)(1)(mat)
    : mat

const getKnightMoves = state => (i, j) => mat => base.pipe(
    checkSquare(state)(i, j, 1, 2),
    checkSquare(state)(i, j, 2, 1),
    checkSquare(state)(i, j, 2, -1),
    checkSquare(state)(i, j, 1, -2),
    checkSquare(state)(i, j, -1, -2),
    checkSquare(state)(i, j, -2, -1),
    checkSquare(state)(i, j, -2, 1),
    checkSquare(state)(i, j, -1, 2),
)(base.replace2d(i, j)('x')(mat))

module.exports = {
    pawn: getPawnMoves,
    rook: getRookMoves,
    bishop: getBishopMoves,
    queen: getQueenMoves,
    knight: getKnightMoves,
}