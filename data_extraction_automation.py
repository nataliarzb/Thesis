# ====================================================================================
# PROJECT: Undergraduate Thesis - Data Extraction Automation (Web Scraping)
# SCRIPT: 05_web_scraping_birdlife.py
# AUTHOR: Natalia Ramírez Bedoya
#
# OBJECTIVE:
# To automate the search and extraction of data in BirdLife DataZone
# using Web Scraping (Selenium/Chrome).

# DESCRIPTION:
# The script automates Chrome navigation to apply specific selection criteria (paramo species) and extract critical information for connectivity analysis.

# #
# TOOLS: Python, Selenium, Openpyxl, Regex (re)
# ====================================================================================


import re 
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from tqdm import tqdm
import time
import csv
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import TimeoutException, NoSuchElementException, StaleElementReferenceException, WebDriverException
import openpyxl
import logging
import os
from datetime import datetime


# =================================================================================
# 1. CONFIGURACIÓN DE REANUDACIÓN (¡AJUSTAR RUTA DEL LOG ANTERIOR!)
# =================================================================================
# Define el nombre del archivo LOG ANTERIOR del que quieres retomar la ejecución.
# ¡REEMPLAZA ESTA RUTA CON LA RUTA REAL DE TU LOG ANTERIOR!
LOG_FILE_TO_RESUME = r"logs\extraccion_aves_colombia_filtrada_20251013_143143.log.log"
# Patrón de éxito que se buscará en ese log.
COMPLETED_PATTERN = re.compile(r" ESPECIE_PROCESADA: (.*)$") 

def load_processed_urls_from_log(log_path):
    """Carga los URLs de especies procesadas del archivo log anterior."""
    processed_urls = set()
    try:
        with open(log_path, 'r', encoding='utf-8') as f:
            for line in f:
                match = COMPLETED_PATTERN.search(line)
                if match:
                    # El grupo 1 (.*) captura la URL que fue registrada
                    processed_urls.add(match.group(1).strip())
        logging.info(f" Se cargaron {len(processed_urls)} URLs previamente procesadas del log anterior.")
        return processed_urls
    except FileNotFoundError:
        logging.warning(f" Archivo de log para reanudar ({log_path}) no encontrado. Iniciando desde cero.")
        return set()
    except Exception as e:
        logging.error(f" Error al leer el log para reanudar: {e}")
        return set()

# =================================================================================
# 2. CONFIGURACIÓN DE LOGGING Y VARIABLES INICIALES
# =================================================================================
LOG_DIR = "logs"
os.makedirs(LOG_DIR, exist_ok=True)
# Se crea un NUEVO log para esta sesión de reanudación
LOG_FILE = os.path.join(LOG_DIR, f"extraccion_aves_ecuador_filtrada_{datetime.now().strftime('%Y%m%d_%H%M%S')}_REANUDACION.log")

logging.basicConfig(level=logging.INFO,
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    handlers=[
                        logging.FileHandler(LOG_FILE, 'w', encoding='utf-8'),
                        logging.StreamHandler()
                    ])
logging.info(f"Guardando la salida de la terminal en: {LOG_FILE}")


# Inicializa el set de URLs ya procesadas leyendo el log anterior
processed_urls = load_processed_urls_from_log(LOG_FILE_TO_RESUME)
processed_species_names = set() # Se mantiene para el control de duplicados interno por nombre
TOTAL_ESPECIES_ESPERADAS = 1628 

# Configurar opciones de Chrome
chrome_options = Options()
chrome_options.add_argument('--ignore-ssl-errors=yes')
chrome_options.add_argument('--allow-insecure-localhost')

# INICIALIZACIÓN CORREGIDA DEL DRIVER (Solución al error SessionNotCreatedException)
try:
    # Selenium Manager encuentra automáticamente el ChromeDriver compatible
    driver = webdriver.Chrome(options=chrome_options)
except WebDriverException as e:
    logging.error(f" Error al iniciar el driver: Asegúrate de que Chrome está cerrado. Error: {e}")
    exit(1)
except Exception as e:
    logging.error(f" Error general al iniciar el driver: {e}")
    exit(1)

# Establecer timeouts
driver.set_page_load_timeout(180) 
driver.set_script_timeout(90)
wait = WebDriverWait(driver, 120)

result_species = []

try:
    # =========================================================================
    # 3. NAVEGACIÓN Y APLICACIÓN DE FILTROS
    # =========================================================================
    driver.get("https://datazone.birdlife.org/species/search")
    time.sleep(10)

    try:
        # GESTIÓN DE COOKIES
        try:
            allow_all_cookies_button = wait.until(EC.element_to_be_clickable((By.XPATH, '//button[contains(text(), "Allow all cookies") or contains(text(), "Aceptar todas las cookies") or contains(text(), "Accept all cookies")]')))
            allow_all_cookies_button.click()
            logging.info(" Se hizo clic en el botón 'Allow all cookies'.")
        except:
            logging.info(" No se encontró o no se pudo hacer clic en el botón de cookies.")
            pass
        try:
            wait.until(EC.invisibility_of_element_located((By.CLASS_NAME, 'CybotCookiebotDialog')))
        except:
            pass

        # Paso 1: Abrir el filtro
        filter_button = wait.until(EC.element_to_be_clickable((By.XPATH, "/html/body/main/div/div[1]/div/div[1]/div[1]/div")))
        filter_button.click()
        logging.info("Botón 'Filter' clickeado.")

        # Paso 2: Hacer clic en "SHOW MORE"
        show_more_button = wait.until(EC.element_to_be_clickable((By.XPATH, "//*[@id='Geographic region']/button")))
        show_more_button.click()
        logging.info(" Botón 'Show more' clickeado.")

        # Paso 3: Seleccionar "South America"
        south_america_option = wait.until(EC.element_to_be_clickable((By.XPATH, '//*[@id="Geographic region"]/div/div[13]/div[1]/div/span')))
        south_america_option.click()
        logging.info("Opción 'South America' seleccionada.")

        # Paso 4: Seleccionar el país (Asumo que el XPATH del país es correcto)
        south_america_country_list_container = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="south america-dropdown"]')))
        driver.execute_script("arguments[0].scrollBy(0, 500);", south_america_country_list_container)
        time.sleep(2)

        venezuela_option = wait.until(EC.element_to_be_clickable((By.XPATH,'//*[@id="south america-dropdown"]/div[6]/label'))) 
        venezuela_option.click()
        logging.info("Opción 'Ecuador' seleccionada.")
        time.sleep(7)

        # Paso 6: Aplicar filtros
        apply_filters_locator = (By.XPATH, "//button[text()='Apply filters']")
        apply_filters_button = wait.until(EC.element_to_be_clickable(apply_filters_locator))
        apply_filters_button.click()
        logging.info(" Botón 'Apply filters' clickeado por texto.")


    except Exception as e:
        logging.error(f" Error durante la aplicación de filtros: {e}")
        raise # Re-lanza para que el bloque principal finally capture el error.
        
    # =========================================================================
    # 4. EXTRACCIÓN Y SCROLL DE ENLACES
    # =========================================================================
    wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="scrollableDiv"]')))
    time.sleep(5)

    enlaces_vistos = set() 
    all_species_links = []
    scrolls = 0
    max_scrolls = 500
    scroll_delay = 30 
    
    with tqdm(total=1628, desc="Cargando enlaces", unit="link") as pbar:
        while len(all_species_links) < 1628 and scrolls < max_scrolls:
            try:
                contenedor_scrollable = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="scrollableDiv"]')))
                
                grid_contenedor = contenedor_scrollable.find_element(By.XPATH, './/div[@class="l:gap-[12px] l:grid-cols-[repeat(auto-fill,minmax(184px,1fr))] grid grid-cols-[repeat(auto-fill,minmax(164px,1fr))] gap-[8px]"]')
                enlaces_actuales = grid_contenedor.find_elements(By.XPATH, './/a')
                
                nuevos_enlaces = 0
                for enlace in enlaces_actuales:
                    href = enlace.get_attribute("href")
                    normalized_href = href.split('?')[0].rstrip('/') if href else None
                    if normalized_href and normalized_href not in enlaces_vistos and "/species/factsheet/" in normalized_href:
                        all_species_links.append(normalized_href)
                        enlaces_vistos.add(normalized_href)
                        pbar.update(1)
                        nuevos_enlaces += 1
                        if len(all_species_links) >= 1386:
                            break
                
                if len(all_species_links) < 1386:
                    driver.execute_script("arguments[0].scrollTop = arguments[0].scrollHeight;", contenedor_scrollable)
                    time.sleep(scroll_delay)
                    scrolls += 1
                else: 
                    logging.info(" Se han encontrado todos los enlaces necesarios.")
                    break
            
            except StaleElementReferenceException:
                logging.warning(" StaleElementReferenceException: Reintentando encontrar el grid_contenedor.")
            except Exception as e:
                logging.error(f" Error al encontrar el grid_contenedor o los enlaces: {e}")
                break
    pbar.close()

    logging.info(f"Se encontraron {len(all_species_links)} enlaces de especies.")
    links = list(set(all_species_links))
    logging.info(f"Tras deduplicación final de enlaces (por URL), se procesarán {len(links)} enlaces únicos.")


    # =========================================================================
    # 5. PROCESAMIENTO DE CADA ESPECIE (CON FILTRO DE REANUDACIÓN)
    # =========================================================================
    for url in tqdm(links, desc="Procesando especies"):
        
        # --- FILTRO DE REANUDACIÓN ---
        if url in processed_urls:
            logging.info(f" Saltando URL: {url} (Ya fue procesada en una sesión anterior).")
            continue
        # -----------------------------

        # Inicializar variables
        name = "Nombre no encontrado"
        habitat_text = "Hábitat no encontrado"
        elevation_text = "Elevación no encontrada"
        normalized_species_name = ""

        try:
            driver.get(url)
            time.sleep(5)
            
            # Extracción del Nombre de la especie
            try:
                name_element = WebDriverWait(driver, 30).until(EC.presence_of_element_located((By.XPATH, "//h1/following-sibling::div")))
                name = name_element.text
                normalized_species_name = name.strip().lower()
                
                if not name or name.lower() in ("nombre no encontrado", "-"):
                    logging.warning(f" Nombre de especie vacío o inválido para {url}. Saltando.")
                    continue
            except (TimeoutException, NoSuchElementException, StaleElementReferenceException) as e:
                logging.warning(f"No se pudo obtener el nombre para {url} después de esperar (Error: {e}). Saltando.")
                continue

            # Extracción de Elevación
            try:
                elemento_elevacion = driver.find_element(By.XPATH, '//*[@id="Ecology"]/div[2]/div/div/div[3]/div/div[2]/h4')
                driver.execute_script("arguments[0].scrollIntoView(true);", elemento_elevacion)
                elev_section = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="Ecology"]/div[2]/div/div/div[3]/div/div[2]/h4')))
                elevation_text = elev_section.text
            except:
                elevation_text = "Elevación no encontrada"

            # Extracción de Hábitat
            try:
                elemento_habitat = driver.find_element(By.XPATH, '//*[@id="Ecology"]/div[3]/div[2]')
                driver.execute_script("arguments[0].scrollIntoView(true);", elemento_habitat)
                habitat_cells = wait.until(EC.visibility_of_all_elements_located((By.XPATH, '//*[@id="Ecology"]/div[3]/div[2]')))
                habitat_text = " ".join([cell.text for cell in habitat_cells])
            except:
                habitat_text = "Hábitat no encontrado"

            logging.info(f"Procesando: {name} - Habitat: {habitat_text}, Elevación: {elevation_text}")


            # --- Lógica de Filtrado por Criterios ---
            meets_criteria = False
            
            # Condición de Hábitat
            if "Shrubland Subtropical/Tropical High Altitude Resident Major" in habitat_text or \
                "Shrubland Subtropical/Tropical High Altitude Breeding Major" in habitat_text or \
                "Shrubland Subtropical/Tropical High Altitude Non-breeding Major" in habitat_text or \
                "Grassland Subtropical/Tropical High Altitude Resident Major" in habitat_text or \
                "Grassland Subtropical/Tropical High Altitude Breeding Major" in habitat_text or \
                "Grassland Subtropical/Tropical High Altitude Non-breeding Major" in habitat_text or \
                "Wetlands (inland)" in habitat_text:
                
                # Condición de Elevación
                if not elevation_text or elevation_text == "Elevación no encontrada" or elevation_text == "-":
                    meets_criteria = True 
                else:
                    meets_elevation_numeric_criteria = False
                    cleaned_elevation_text = "".join(filter(lambda x: x.isdigit() or x == '-' or x == ',', elevation_text))
                    parsed_elevations = []
                    parts = cleaned_elevation_text.split('-')
                    for part in parts:
                        try:
                            parsed_elevations.append(int(part.replace(",", "")))
                        except ValueError:
                            continue 

                    if parsed_elevations:
                        if len(parsed_elevations) >= 2:
                             if max(parsed_elevations) >= 3000:
                                 meets_elevation_numeric_criteria = True
                        elif len(parsed_elevations) == 1:
                            if parsed_elevations[0] >= 3000:
                                meets_elevation_numeric_criteria = True
                        elif any(e >= 3000 for e in parsed_elevations):
                            meets_elevation_numeric_criteria = True
                            
                    if meets_elevation_numeric_criteria:
                        meets_criteria = True
            
            # Si cumple los criterios, se añade al resultado final
            if meets_criteria:
                result_species.append({'nombre': name, 'habitat': habitat_text, 'elevacion': elevation_text})
                logging.info(f" Especie '{name}' agregada a la lista final (cumple criterios). Total: {len(result_species)}")
            else:
                logging.info(f" Especie '{name}' NO agregada a la lista final (no cumple criterios).")

            # === REGISTRO DE CHECKPOINT ===
            processed_urls.add(url)
            logging.info(f" ESPECIE_PROCESADA: {url}") # <-- Línea para que la función de reanudación la detecte

        except Exception as e:
            logging.error(f"Error general al procesar la URL {url} para la especie {name}: {e}")
            # NO se registra el checkpoint, por lo que se reintentará en la próxima sesión.
            try:
                driver.get("https://datazone.birdlife.org/species/search")
                wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="scrollableDiv"]')))
                time.sleep(3)
            except:
                pass 
            continue 


    # Guardar en archivo Excel
    libro_excel = openpyxl.Workbook()
    hoja_activa = libro_excel.active
    hoja_activa.append(['nombre', 'habitat', 'elevacion']) 

    for especie in result_species:
        hoja_activa.append([especie['nombre'], especie['habitat'], especie['elevacion']])

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    nombre_archivo_excel = f"especies_filtradas_ecuador_nuevos_criterios_{timestamp}.xlsx"
    
    libro_excel.save(nombre_archivo_excel)
    logging.info(f"\n Resultados guardados en {nombre_archivo_excel}")

except TimeoutException as te:
    logging.error(f" Error de Timeout: {te}")
except Exception as e:
    logging.error(f"Error general en el bloque principal: {e}")

finally:
    if 'driver' in locals() and driver:
        driver.quit()
