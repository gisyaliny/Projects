import streamlit as st
import streamlit.components.v1 as components
from geopy.geocoders import Nominatim

def esri_search_component(key=None):
    component_path = "./esri_search_component.html"
    component = components.declare_component("esri_search_component", path=component_path)
    return component(key=key)

def app():
    st.title("Location Search")

    location = esri_search_component(key="location_search")

    if location is not None:
        address, lat, lon = location

        if lat is None or lon is None:
            if address == "Address not found.":
                # Use geopy for zip code search
                geolocator = Nominatim(user_agent="location_search")
                try:
                    location = geolocator.geocode(address)
                    if location:
                        lat, lon = location.latitude, location.longitude
                        st.success(f"Latitude: {lat}, Longitude: {lon}")
                    else:
                        st.warning("Location not found.")
                except Exception as e:
                    st.error("Error occurred during search.")
            else:
                st.warning(address)
        else:
            st.success(f"Latitude: {lat}, Longitude: {lon}")

if __name__ == "__main__":
    app()