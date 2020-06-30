const zeroBoard = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0]
]

const flip = mat => mat.reverse().forEach(arr => arr.reverse())

const getCodeAt = state => (i, j) => state.board[i] && state.board[i][j]
const getTeamCode = x => x && x.toUpperCase
    ? x.toUpperCase() == x
    : undefined

const pieceToMove = state => getCodeAt(state)(...state.move[0])
const pieceAtMoveEnd = state => getCodeAt(state)(...state.move[1])

const areSameTeam = state => (i1, j1) => (i2, j2) => getTeamCode(getCodeAt(state)(i1, j1)) == getTeamCode(getCodeAt(state)(i2, j2))
const areDifferentTeam = state => (i1, j1) => (i2, j2) => {
    const c1 = getTeamCode(getCodeAt(state)(i1, j1))
    const c2 = getTeamCode(getCodeAt(state)(i2, j2))
    console.log(i1, j1, i2, j2, c1, c2)
    if(c1 == undefined || c2 == undefined) return false

    return c1 != c2
}

const isWhite = x => x.toUpperCase() == x
const isBlack = x => x.toLowerCase() == x

module.exports = {
    ZEROS: zeroBoard,

    flip,
    getCodeAt,
    getTeamCode,

    pieceToMove,
    pieceAtMoveEnd,

    areSameTeam,
    areDifferentTeam,

    isWhite,
    isBlack
}