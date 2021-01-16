// The main Riddler Classic problem from January 8, 2021
// For each shot on target, decide whether the game continues or ends
// Return the total number of shots divided by the total number of games

function targetShot(games) {
    let gameTurns = 0;
    let gamesCompleted = 0;
    let lastDistance = 100;

    while (gamesCompleted < games) {
        // randomly choose a point where each shot lands
        let shotX = Math.random();
        let shotY = Math.random();
        let distance = Math.sqrt(shotX**2 + shotY**2);
        if (distance > 1) {
            // the problem states that an arrow never misses the target
            continue;
        }
        if (distance < lastDistance) {
            // the game continues
            gameTurns++;
            lastDistance = distance;
        } else {
            // the game ends
            gameTurns++;
            gamesCompleted++;
            lastDistance = 100;
        }
    }

    return gameTurns / gamesCompleted;
}

//Uncomment the next line to view the result
//console.log(targetShot(1000000));


// The Extra Credit
function targetShotTen(games) {
    let gameTurns = 0;
    let gamesCompleted = 0;
    let lastDistance = 100;

    while (gamesCompleted < games) {
        // randomly choose a point where each shot lands
        let shotX = Math.random();
        let shotY = Math.random();
        let distance = Math.sqrt(shotX**2 + shotY**2);
        if (distance > 1) {
            // the problem states that an arrow never misses the target
            continue;
        } else {
            // if the shot is on target, compute the circle it landed in
            distance = Math.ceil(distance * 10)
        }
        if (distance < lastDistance) {
            // the game continues
            gameTurns++;
            lastDistance = distance;
        } else {
            // the game ends
            gameTurns++;
            gamesCompleted++;
            lastDistance = 100;
        }
    }

    return gameTurns / gamesCompleted;
}

//Uncomment the next line to view the result
//console.log(targetShotTen(1000000));