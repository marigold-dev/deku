import { DekuPClient } from "@marigold-dev/deku"

const run = async ({allDekuRpc}) => {
    const nodes = allDekuRpc.map(dekuRpc => new DekuPClient({dekuRpc: dekuRpc}))
    // get the level for each node
    const levels = await Promise.all(nodes.map(node => node.level()));
    // Get the mean level
    const mean = levels.reduce((acc, level) => acc + level, 0) / levels.length;
    // Compute the sum of (x - mean)²
    const sum = levels
        .map(level => (level - mean) ** 2)
        .reduce((acc, curr) => acc + curr, 0)
    // Compute the variance: sum / n
    let variance = sum / levels.length;
    // Compute the deviation: sqrt(variance)
    let deviation = Math.sqrt(variance);
    if(deviation > 1) throw "The API is not sync"
    return "The API is sync"
}

export default {
    run
}