import { Cell as CellType, CellId, step } from "../core"

interface CellProperty {
    id: CellId,
    state: CellType,
    onClick: ({ id, state }: { id: CellId, state: CellType }) => void
    step: step
}

const Cell = ({ id, state, onClick, step }: CellProperty) => {
    const character = state === "Circle"
        ? "O"
        : state === "Cross"
            ? "X"
            : ""

    const className = step.type === "YOUR_TURN" ? "clickable" : "";

    return (
        <div className={`cell ${className}`} onClick={() => onClick({ id, state })}>{character}</div>);
}

export default Cell