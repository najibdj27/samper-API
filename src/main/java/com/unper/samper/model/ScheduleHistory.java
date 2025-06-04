package com.unper.samper.model;

import java.util.Calendar;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.unper.samper.model.common.Audit;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "schedule_history", schema = "schedule")
public class ScheduleHistory extends Audit {
    @Id
    @GeneratedValue(strategy =  GenerationType.IDENTITY)
    private Long id;

    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, targetEntity = Schedule.class)
    @JoinColumn(name = "schedule_id")
    private Schedule schedule;

    private Calendar openTime;
    
    private Calendar closeTime;

    private Double openLongitude;

    private Double openLatitude;

    private Double closeLongitude;

    private Double closeLatitude;
}
