-- Select data that we will be using
SELECT Location, date, total_cases, new_cases, total_deaths, population
FROM PortfolioProject..CovidDeaths$
ORDER BY 1,2


--Looking at total cases vs. total deaths
SELECT Location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 AS DeathPercentage
FROM PortfolioProject..CovidDeaths$
ORDER BY 1,2

-- Looking at the toal cases vs. Population
SELECT Location, date, total_cases, population, (total_cases/population) * 100 AS ContractedPercentage
FROM PortfolioProject..CovidDeaths$
ORDER BY 1,2

-- Looking at countries with the highest infection rate compared to population
SELECT Location, population, MAX(total_cases) as HighestInfectionCount, (MAX(total_cases)/population)*100 AS ContractedPercentage
FROM PortfolioProject..CovidDeaths$
GROUP BY location,population
ORDER BY ContractedPercentage DESC

-- Showing countries with the highest death count per population
SELECT Location, MAX(cast(total_deaths as int)) as TotalDeathCount
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
GROUP BY location
ORDER BY TotalDeathCount DESC

-- Breaking things down by continent 
SELECT continent, MAX(cast(total_deaths as int)) as TotalDeathCount
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
GROUP BY continent
ORDER BY TotalDeathCount DESC

--Showing the continents with the highest death count
SELECT continent, MAX(cast(total_deaths as int)) as TotalDeathCount
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
GROUP BY continent
ORDER BY TotalDeathCount DESC

--Showing global death percentages
SELECT  date, SUM(new_cases) AS TotalCases, SUM(cast (new_deaths as int)) AS TotalDeaths, SUM(cast (new_deaths as int))/SUM(new_cases) * 100 AS DeathPercentage
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
GROUP BY date
ORDER BY 1,2

-- toal global death percentage
SELECT  date, SUM(new_cases) AS TotalCases, SUM(cast (new_deaths as int)) AS TotalDeaths, SUM(cast (new_deaths as int))/SUM(new_cases) * 100 AS DeathPercentage
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
--GROUP BY date
ORDER BY 1,2

-- Looking at Total Population vs. Vaccinations

SELECT D.continent, D.location, D.date, D.population, V.new_vaccinations
, SUM(CONVERT(bigint,V.new_vaccinations)) OVER (Partition by D.location ORDER BY D.Location, D.date) AS RollingPeopleVaccinated
FROM PortfolioProject..CovidVaccinations$ V
JOIN PortfolioProject..CovidDeaths$ D
	ON V.location = D.location
	and V.date = D.date
WHERE d.continent is not null
ORDER BY 2,3

-- Use CTE
WITH PopVsVac (Continent, location, date, population, new_vaccinations, RollingPeopleVaccinated)

AS
(
SELECT D.continent, D.location, D.date, D.population, V.new_vaccinations
, SUM(CONVERT(bigint,V.new_vaccinations)) OVER (Partition by D.location ORDER BY D.Location, D.date) AS RollingPeopleVaccinated
FROM PortfolioProject..CovidVaccinations$ V
JOIN PortfolioProject..CovidDeaths$ D
	ON V.location = D.location
	and V.date = D.date
WHERE d.continent is not null

)
SELECT *, (RollingPeopleVaccinated/population) * 100
FROM PopVsVac

-- Temp Table

DROP table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
New_Vaccinations numeric,
RollingPeopleVaccinated numeric
)

INSERT INTO #PercentPopulationVaccinated

SELECT D.continent, D.location, D.date, D.population, V.new_vaccinations
, SUM(CONVERT(bigint,V.new_vaccinations)) OVER (Partition by D.location ORDER BY D.Location, D.date) AS RollingPeopleVaccinated
FROM PortfolioProject..CovidVaccinations$ V
JOIN PortfolioProject..CovidDeaths$ D
	ON V.location = D.location
	and V.date = D.date
WHERE d.continent is not null

SELECT *, (RollingPeopleVaccinated/population) * 100
FROM #PercentPopulationVaccinated

--Creating view to store date for later visualizations

CREATE view PercentPopulationVaccinated AS

SELECT D.continent, D.location, D.date, D.population, V.new_vaccinations
, SUM(CONVERT(bigint,V.new_vaccinations)) OVER (Partition by D.location ORDER BY D.Location, D.date) AS RollingPeopleVaccinated
FROM PortfolioProject..CovidVaccinations$ V
JOIN PortfolioProject..CovidDeaths$ D
	ON V.location = D.location
	and V.date = D.date
WHERE d.continent is not null
--ORDER BY 2,3

SELECT *
FROM PercentPopulationVaccinated