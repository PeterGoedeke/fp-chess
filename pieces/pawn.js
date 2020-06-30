const base = require('../base')
const board = require('../board')

const getValidMatrix = state => (i, j) => base.pipe(
    checkForwardTiles(state)(i, j),
    checkPawnCapture(state)(i, j, 1),
    checkPawnCapture(state)(i, j, -1),
    // checkEnPassant(state)(i, j)
)(base.replace2d(i, j)('x')(board.ZEROS))

const checkForwardTiles = state => (i, j) => mat => board.getCodeAt(state)(i, j + 1) == 0 // can move one?
    ? j == 1 && board.getCodeAt(state)(i, j + 2) == 0 // can move two?
        ? base.replaceBetween(i, j + 1)(i, j + 3)(1)(mat)
        : base.replace2d(i, j + 1)(1)(mat)
    : mat

const checkPawnCapture = state => (i, j, dir) => mat => board.areDifferentTeam(state)(i, j)(i + dir, j + 1)
    ? base.replace2d(i + dir, j + 1)(1)(mat)
    : mat

// const checkEnPassant = state => (i, j, dir) => mat => 

// const pawnCheck

module.exports = {
    getValidMatrix,
    checkForwardTiles
}