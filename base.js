const getUnitVector = (i1, j1) => (i2, j2) => {
    const magnitude = Math.sqrt(Math.pow(i2 - i1, 2) + Math.pow(j2 - j1, 2))
    return [Math.round((i2 - i1) / magnitude), Math.round((j2 - j1) / magnitude)]
}
const pointEq = p1 => p2 => p1.i == p2.i && p1.j == p2.j

const merge = o1 => o2 => Object.assign({}, o1, o2)
const prop = k => o => o[k]
const pipe = (...fns) => x => [...fns].reduce((acc, f) => f(acc), x)

const replace = arr => i => x => Object.assign([], arr, { [i]: x })
const replace2d = (i, j) => x => mat => Object.assign([], mat, { [i]: replace(mat[i])(j)(x) })

const replaceBetween = (i1, j1) => (i2, j2) => x => mat => {
    if(pointEq({i: i1, j: j1})({i: i2, j: j2})) return mat

    const v = getUnitVector(i1, j1)(i2, j2)
    return replaceBetween(i1 + v[0], j1 + v[1])(i2, j2)(x)(replace2d(i1, j1)(x)(mat))
}

const pipeLog = (x, ...y) => (console.log(x, ...y), x)
const andLog = (x, ...y) => (console.log(x, ...y), true)

module.exports = {
    getUnitVector,
    pointEq,
    
    merge,
    prop,
    pipe,

    replace,
    replace2d,
    replaceBetween,

    pipeLog,
    andLog
}