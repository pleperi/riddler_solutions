// Riddler Classic problem from February 5, 2021
function tower_of_hanoi(trials, disks, rods) {
    let tower = [];
    for (let i = 1; i <= disks; i++) {
        tower.push(i);
    }

    let stackA;
    let stackB;
    let stacks = {1: tower}

    let base = 1;
    let turns = 0;
    let completedShifts = 0;

    while(completedShifts < trials) {
        stackA = Math.ceil(Math.random() * rods)
        stackB = Math.ceil(Math.random() * rods)

        if (stackA == stackB || (stacks[stackA] == undefined && stacks[stackB] == undefined)) {
            continue;
        }
        turns++;

        if (stacks[stackA] == undefined) {
            stacks[stackA] = [stacks[stackB].shift()]
        } else if (stacks[stackB] == undefined) {
            stacks[stackB] = [stacks[stackA].shift()]
        } else if (stacks[stackA][0] > stacks[stackB][0]) {
            let diskToMove = stacks[stackB].shift();
            stacks[stackA].unshift(diskToMove);
            if (stacks[stackA].length == disks && stackA != base) {
                completedShifts++;
                base = stackA;
            }
        } else if (stacks[stackB][0] > stacks[stackA][0]) {
            let diskToMove = stacks[stackA].shift();
            stacks[stackB].unshift(diskToMove);
            if (stacks[stackB].length == disks && stackB != base) {
                completedShifts++;
                base = stackB;
            }
        }
        if (stacks[stackA].length == 0) {
            stacks[stackA] = undefined;
        }
        if (stacks[stackB].length == 0) {
            stacks[stackB] = undefined;
        }
    }
    return (turns/completedShifts);
}

console.log(tower_of_hanoi(100000, 3, 3))
// for 3 disks, the mean value is about 70.7
// for 4 disks, the mean value is about 402.6
// for 5 disks, the mean value is about 2155.7