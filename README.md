# Welfare Policy R Shiny App

This R Shiny application provides an interactive interface to manage and track employee welfare contributions and events. The application allows users to input various parameters, such as the number of employees, contribution amounts, and specific events, to generate a detailed monthly contribution table.

## Features

- **Custom Theme**: Uses a custom theme with `bslib` for a modern, dark-themed UI.
- **Interactive Navbar**: Features an interactive navbar with a toggle for input controls.
- **Sidebar Navigation**: Contains a navigation menu and a custom footer within the sidebar.
- **Input Controls**: Allows users to input various parameters, including the number of employees, contribution amounts, and specific events.
- **Dynamic Event Handling**: Dynamically generates event descriptions and amounts based on user input.
- **Contribution Table**: Displays a detailed contribution table with monthly and total contributions, event descriptions, and financial summaries.
- **JavaScript Formatting**: Uses JavaScript to format numeric columns and style the table for better readability.

## Usage

1. **Clone the Repository**:
   ```sh
   git clone https://github.com/yourusername/welfare-policy-app.git
   cd welfare-policy-app

2. **Install Dependencies**:
Ensure you have the required R packages installed:

```sh
install.packages(c("shiny", "DT", "bs4Dash", "bslib", "scales", "magrittr"))

2. **Run the Application**:
Open the R project and run the application:

Parameters
Number of Employees: Input the total number of employees.
Contribution Amount: Specify the monthly contribution amount per employee.
Select Month: Choose the month for which you want to update events.
Balance Brought Forward: Input any balance brought forward for January.
Event Descriptions: Select up to three events (Birth, Wedding, Loss of a Loved One, No Incident, Exit Package).
Amount Invested: Enter the amount invested.
Investment Return: Specify the return on investment.
Contributing
Feel free to submit issues or pull requests for any enhancements or bug fixes.

License
This project is licensed under the MIT License. See the LICENSE file for details.

Contact
For any questions or feedback, please contact [robinochieng73@gmail.com].
