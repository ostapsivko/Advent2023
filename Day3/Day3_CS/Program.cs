Part1();

Part2();

void Part2()
{
    var lines = File.ReadAllLines("input.txt");

    var currentValue = 0;

    for (var i = 0; i < lines.Length; i++)
    {
        for(var j = 0; j < lines[i].Length; j++)
        {
            if(!char.IsDigit(lines[i][j]) && IsStar(lines[i][j]))
            {
                if(HasTwoAdjacentNumbers(lines, i, j, out var value))
                    currentValue += value;
            }
        }
    }

    Console.WriteLine(currentValue);
}

bool IsStar(char symb) => symb == '*';

bool HasTwoAdjacentNumbers(string[] lines, int i, int j, out int value)
{
    value = 1;
    int numbersValue = 0;

    if(i != 0)
    {
        if(char.IsDigit(lines[i - 1][j]))
        {
            numbersValue++;
            value *= GetNumber(lines[i - 1], j, j);
        }
        else 
        {
            if(j != 0 && char.IsDigit(lines[i - 1][j - 1]))
            {
                numbersValue++; 
                value *= GetNumber(lines[i - 1], j - 1, j - 1);
            }

            if(j != lines[i].Length - 1 && char.IsDigit(lines[i - 1][j + 1]))
            {
                if(numbersValue == 2)
                    return false;

                numbersValue++; 
                value *= GetNumber(lines[i - 1], j + 1, j + 1);
            }
        }
    }

    if(j != 0 && char.IsDigit(lines[i][j - 1]))
    {
        if(numbersValue == 2)
            return false;

        numbersValue++; 
        value *= GetNumber(lines[i], j - 1, j - 1);
    }

    if(j != lines[i].Length - 1 && char.IsDigit(lines[i][j + 1]))
    {
        if(numbersValue == 2)
            return false;

        numbersValue++; 
        value *= GetNumber(lines[i], j + 1, j + 1);
    }

    if(i != lines.Length - 1)
    {
        if(char.IsDigit(lines[i + 1][j]))
        {
            if(numbersValue == 2)
                return false;

            numbersValue++; 
            value *= GetNumber(lines[i + 1], j, j);
        }
        else
        {
            if(j != 0 && char.IsDigit(lines[i + 1][j - 1]))
            {
                if(numbersValue == 2)
                    return false;

                numbersValue++; 
                value *= GetNumber(lines[i + 1], j - 1, j - 1);
            }

            if(j != lines[i].Length - 1 && char.IsDigit(lines[i + 1][j + 1]))
            {
                if(numbersValue == 2)
                    return false;

                numbersValue++; 
                value *= GetNumber(lines[i + 1], j + 1, j + 1);
            }
        }
    }

    return numbersValue == 2;
}

int AddAdjacentNumbers(string[] lines, int i, int j)
{
    int currentValue = 0;

    if(i != 0)
    {
        if(char.IsDigit(lines[i - 1][j]))
            currentValue += GetNumber(lines[i - 1], j, j);
        else 
        {
            if(j != 0 && char.IsDigit(lines[i - 1][j - 1]))
                currentValue += GetNumber(lines[i - 1], j - 1, j - 1);

            if(j != lines[i].Length - 1 && char.IsDigit(lines[i - 1][j + 1]))
                currentValue += GetNumber(lines[i - 1], j + 1, j + 1);
        }
    }

    if(j != 0 && char.IsDigit(lines[i][j - 1]))
        currentValue += GetNumber(lines[i], j - 1, j - 1);

    if(j != lines[i].Length - 1 && char.IsDigit(lines[i][j + 1]))
        currentValue += GetNumber(lines[i], j + 1, j + 1);

    if(i != lines.Length - 1)
    {
        if(char.IsDigit(lines[i + 1][j]))
            currentValue += GetNumber(lines[i + 1], j, j);
        else
        {
            if(j != 0 && char.IsDigit(lines[i + 1][j - 1]))
                currentValue += GetNumber(lines[i + 1], j - 1, j - 1);

            if(j != lines[i].Length - 1 && char.IsDigit(lines[i + 1][j + 1]))
                currentValue += GetNumber(lines[i + 1], j + 1, j + 1);
        }
    }

    return currentValue;
}

void Part1()
{
    var lines = File.ReadAllLines("test_input.txt");

    var currentValue = 0;

    for (var i = 0; i < lines.Length; i++)
    {
        for(var j = 0; j < lines[i].Length; j++)
        {
            if(!char.IsDigit(lines[i][j]) && lines[i][j] != '.')
            {
                currentValue += AddAdjacentNumbers(lines, i, j);
            }
        }
    }

    Console.WriteLine(currentValue);
}

int GetNumber(string input, int start, int end)
{
    while(start > 0 && char.IsDigit(input[start - 1]))
        start--;

    while(end < input.Length - 1 && char.IsDigit(input[end + 1]))
        end++;
    
    var result = int.Parse(input.Substring(start, end - start + 1));

    return result;
}