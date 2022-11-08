import { Cell as CellType, CellId } from "../core";
import Cell from "./Cell"

interface BoardProperty {
    cells: Array<CellType>,
    onCellClick: ({ id, state }: { id: CellId, state: CellType }) => void
}

const Board = ({ cells, onCellClick }: BoardProperty) => {
    return (
        <div id="game">
            {cells.map((state, i) => <Cell id={i} key={i} state={state} onClick={onCellClick} />)}
        </div>
    );
}

export default Board