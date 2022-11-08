import { Cell as CellType, CellId } from "../core"

interface CellProperty {
    id: CellId,
    state: CellType,
    onClick: ({ id, state }: { id: CellId, state: CellType }) => void
}

const Cell = ({ id, state, onClick }: CellProperty) => {
    const character = state === "Circle"
        ? "O"
        : state === "Cross"
            ? "X"
            : ""
    return (
        <div className="cell" onClick={() => onClick({ id, state })}>{character}</div>);
}

export default Cell