package com.unper.samper.util;

public class GeoUtils {
    // Radius of the Earth in kilometers
    private static final double EARTH_RADIUS_KM = 6371.0;

    /**
     * Calculates the distance between two points in kilometers using the Haversine formula
     */
    public static Double haversine(Double lat1, Double lon1, Double lat2, Double lon2) {
        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);

        lat1 = Math.toRadians(lat1);
        lat2 = Math.toRadians(lat2);

        double a = Math.pow(Math.sin(dLat / 2), 2)
                + Math.cos(lat1) * Math.cos(lat2)
                * Math.pow(Math.sin(dLon / 2), 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
        return EARTH_RADIUS_KM * c;
    }

    /**
     * Checks if the point (lat2, lon2) is within a given radius (in km) of (lat1, lon1)
     */
    public static Boolean isWithinRadius(Double lat1, Double lon1, Double lat2, Double lon2, Double radiusKm) {
        return haversine(lat1, lon1, lat2, lon2) <= radiusKm;
    }
}
