let gameState = null;

// Initialize the game
async function initGame() {
    try {
        const response = await fetch('/api/game');
        gameState = await response.json();
        renderBoard();
        updateStatus();
    } catch (error) {
        console.error('Error initializing game:', error);
    }
}

// Create a new game
async function newGame() {
    try {
        const width = 10;
        const height = 10;
        const mines = 15;
        
        const formData = new FormData();
        formData.append('width', width);
        formData.append('height', height);
        formData.append('mines', mines);
        
        const response = await fetch('/api/game/new', {
            method: 'POST',
            body: formData,
        });
        
        gameState = await response.json();
        renderBoard();
        updateStatus();
    } catch (error) {
        console.error('Error creating new game:', error);
    }
}

// Reveal a cell
async function revealCell(x, y) {
    if (!gameState || gameState.status !== 'in-progress') return;
    
    try {
        const formData = new FormData();
        formData.append('x', x);
        formData.append('y', y);
        formData.append('game', JSON.stringify(gameState));
        
        const response = await fetch('/api/game/reveal', {
            method: 'POST',
            body: formData,
        });
        
        gameState = await response.json();
        renderBoard();
        updateStatus();
    } catch (error) {
        console.error('Error revealing cell:', error);
    }
}

// Flag a cell
async function flagCell(x, y) {
    if (!gameState || gameState.status !== 'in-progress') return;
    
    try {
        const formData = new FormData();
        formData.append('x', x);
        formData.append('y', y);
        formData.append('game', JSON.stringify(gameState));
        
        const response = await fetch('/api/game/flag', {
            method: 'POST',
            body: formData,
        });
        
        gameState = await response.json();
        renderBoard();
        updateStatus();
    } catch (error) {
        console.error('Error flagging cell:', error);
    }
}

// Render the game board
function renderBoard() {
    if (!gameState) return;
    
    const board = document.getElementById('board');
    board.innerHTML = '';
    
    // Set the grid columns
    board.style.gridTemplateColumns = `repeat(${gameState.width}, 1fr)`;
    
    // Create cells
    for (let y = 0; y < gameState.height; y++) {
        for (let x = 0; x < gameState.width; x++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            
            const pos = `${x},${y}`;
            const cellState = gameState.cells[pos];
            
            if (cellState.type === 'hidden') {
                cell.classList.add('hidden');
                cell.addEventListener('click', () => revealCell(x, y));
                cell.addEventListener('contextmenu', (e) => {
                    e.preventDefault();
                    flagCell(x, y);
                });
            } else if (cellState.type === 'flagged') {
                cell.classList.add('flagged');
                cell.textContent = '🚩';
                cell.addEventListener('contextmenu', (e) => {
                    e.preventDefault();
                    flagCell(x, y);
                });
            } else if (cellState.type === 'revealed') {
                cell.classList.add('revealed');
                if (cellState.adjacentMines > 0) {
                    cell.textContent = cellState.adjacentMines;
                    cell.classList.add(`num-${cellState.adjacentMines}`);
                }
            } else if (cellState.type === 'mine') {
                cell.classList.add('mine');
                cell.textContent = '💣';
            }
            
            board.appendChild(cell);
        }
    }
}

// Update game status
function updateStatus() {
    if (!gameState) return;
    
    const mineCounter = document.getElementById('mine-counter');
    mineCounter.textContent = `Mines: ${gameState.mineCount}`;
    
    const statusElement = document.getElementById('game-status');
    if (gameState.status === 'won') {
        statusElement.textContent = 'You Won! 🎉';
    } else if (gameState.status === 'lost') {
        statusElement.textContent = 'Game Over! 💥';
    } else {
        statusElement.textContent = 'In Progress';
    }
}

// Event listeners
document.addEventListener('DOMContentLoaded', () => {
    initGame();
    
    document.getElementById('new-game').addEventListener('click', newGame);
    
    // Prevent context menu on right click
    document.addEventListener('contextmenu', (e) => {
        if (e.target.classList.contains('cell')) {
            e.preventDefault();
        }
    });
});