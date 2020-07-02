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

const getSimpleKingMoves = state => (i, j) => mat => base.pipe(
    checkSquare(state)(i, j, 1, -1),
    checkSquare(state)(i, j, 1, 0),
    checkSquare(state)(i, j, 1, 1),
    checkSquare(state)(i, j, -1, -1),
    checkSquare(state)(i, j, -1, 0),
    checkSquare(state)(i, j, -1, 1),
    checkSquare(state)(i, j, 0, 1),
    checkSquare(state)(i, j, 0, -1),
)(mat)

const getKingMoves = state => (i, j) => mat => {
    const attackedSquares = getAttackedSquares(state)(board.getOpposingTeamAt(state)(i, j))
    return getSimpleKingMoves(state)(i, j)(mat).map((row, rowIndex) => row.map((col, colIndex) => col && !attackedSquares[rowIndex][colIndex] ? 1 : 0))
}

// const applyMovesOfType = (state) => (i, j) => 

const getPieceMoves = state => (i, j) => mat => board.is.pawn(state)(i, j)
    ? getPawnMoves(state)(i, j)(mat)
    : board.is.bishop(state)(i, j)
    ? getBishopMoves(state)(i, j)(mat)
    : board.is.knight(state)(i, j)
    ? getKnightMoves(state)(i, j)(mat)
    : board.is.rook(state)(i, j)
    ? getRookMoves(state)(i, j)(mat)
    : board.is.queen(state)(i, j)
    ? getQueenMoves(state)(i, j)(mat)
    : board.is.king(state)(i, j)
    ? getSimpleKingMoves(state)(i, j)(mat)
    : mat

const getPieceMovesIfTeam = state => (i, j) => mat => teamCode => board.teamAtIs(state)(i, j)(teamCode)
    ? getPieceMoves(state)(i, j)(mat)
    : mat

const getAttackedSquares = state => teamCode => state.board.reduce((acc, cur, i) =>
    cur.reduce((rowAcc, _, j) => getPieceMovesIfTeam(state)(i, j)(rowAcc)(teamCode), acc), board.ZEROS)

const kingOfTeamChecked = state => teamCode => {
    const { i, j } = board.findKing(state)(teamCode)
    return getAttackedSquares(state)(!teamCode)[i][j] == 1
}

// const getAttackedSquares = state => teamCode => (i = 0, j = 0) => mat => !base.atMatrixEnd(i, j)(mat)
//     ? getAttackedSquares board.teamAtIs(state)(i, j)(!teamCode)
//     : mat

// board.teamAtIs(state)(i, j)(!teamCode)
//     ? getAttackedSquares applyMovesOfType(state)(i, j)(mat)
    // :

module.exports = {
    pawn: getPawnMoves,
    rook: getRookMoves,
    bishop: getBishopMoves,
    queen: getQueenMoves,
    knight: getKnightMoves,
    king: getKingMoves,
    at: getPieceMoves,
    getAttackedSquares,
    kingOfTeamChecked
}